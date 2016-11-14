#!/usr/bin/env lumo
;;
;; Converts Rabobank SEPA CSV-file format (as exported by Rabobank
;; internet banking) to an KMyMoney importable format.
;;
;; Version 0.1.6
;;
;; DISCLAIMER: THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
;; OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;; DAMAGE.
;;
;; Twitter: @maridonkers | Google+: +MariDonkers | GitHub: maridonkers
;;
(ns rabobank.sepa
  (:require [clojure.string :as str]))

;; ---------------
;; NODE.JS INTEROP

(def fs (js/require "fs"))
(def path (js/require "path"))
(def process (js/require "process"))

;; -------------
;; RABOBANK SEPA
;;
;; Beschrijving invoerformaat (Rabobank SEPA CSV-bestand)
;;
;; Veld	Omschrijving			Type		Lengte	Inhoud/Toelichting
;; 1	REKENINGNUMMER_REKENINGHOUDER	Alfanumeriek	35	Eigen Rekeningnummer in IBAN formaat
;; 2	MUNTSOORT			Alfanumeriek	3	EUR
;; 3	RENTEDATUM			Numeriek	8	Formaat: EEJJMMDD
;; 4	BY_AF_CODE			Alfanumeriek	1	D of C
;; 5	BEDRAG				Numeriek	14	2 decimalen. Decimalen worden weergegeven met een punt
;; 6	TEGENREKENING			Alfanumeriek	35	Tegenrekeningnummer
;; 7	NAAR_NAAM			Alfanumeriek	70	Naam van de tegenrekeninghouder
;; 8	BOEKDATUM			Numeriek	8	Formaat: EEJJMMDD
;; 9	BOEKCODE			Alfanumeriek	2	Type transactie
;; 10	FILLER				Alfanumeriek	6	
;; 11	OMSCHR1				Alfanumeriek	35	
;; 12	OMSCHR2				Alfanumeriek	35	
;; 13	OMSCHR3				Alfanumeriek	35	
;; 14	OMSCHR4				Alfanumeriek	35	
;; 15	OMSCHR5				Alfanumeriek	35	
;; 16	OMSCHR6				Alfanumeriek	35	
;; 17	END_TO_END_ID			Alfanumeriek	35	SEPA Credit Transfer: Kenmerk opgegeven door de opdrachtgever
;; 18	ID_TEGENREKENINGHOUDER		Alfanumeriek	35	SEPA Credit Transfer: Identificatie van de tegenrekeninghouder
;; 19	MANDAAT_ID			Alfanumeriek	35	SEPA Direct Debet: Kenmerk machtiging
;;

;; ---------
;; KYMYMONEY
;;
;; Beschrijving uitvoerformaat (KMyMoney import formaat).
;;
;; Veld	Omschrijving			Type
;; 1	NUMBER				Alfanumeriek
;; 2	RENTEDATUM			Numeriek
;; 3	AFSCHRIJVING (Debet)		Numeriek
;; 4	BIJSCHRIJVING (Credit)		Numeriek
;; 5	BOEKCODE (Category)		Alfanumeriek
;; 6	NAAR_NAAM (Payee)		Alfanumeriek
;; 7	OMSCHRIJVING (Memo)		Alfanumeriek
;;
;; The OMSCHRIJVING (Memo) field in the output is a concatenation of
;; fields 11 to 16 plus space separated fields 17 to 19 from the SEPA
;; input. The decimal point in the amounts is replaced with a comma.
;;

;; Set with output filenames, which is used to delete existing files
;; only once.
(def output-fnames (atom #{}))

(defn get-csvs
  "Gets CSV columns as vector. The enclosing quotes are
  removed. Nested quotes are not allowed in Rabobank SEPA."
  [csv-line]
  (map second (re-seq #"\"([^\"]*)\"" csv-line)))

(defn  convert-description
  "Converts description."
  [cvs]

  (let [[_ _ _ _ _ tegenrekening naar-naam _ _ _ & info] cvs
        [_ _ _ _ _ _ end-to-end-id id-tegenrekeninghouder mandaat-id] info

        omschr (drop-last 3 info)
        omschr1 (first omschr)
        omschr (rest omschr)
        
        omschr (->> omschr
                    (map str/trim)
                    (cons (when (seq naar-naam) omschr1))
                    (filter #(seq %))
                    (apply str)
                    str/trim)
        
        extra (->> [end-to-end-id id-tegenrekeninghouder mandaat-id]
                   (map str/trim)
                   (interpose " ")
                   (filter #(seq %))
                   (apply str)
                   str/trim)]

    (str (when (seq tegenrekening) (str "[" tegenrekening "] "))
         (str omschr " " extra))))

(defn convert-columns
  "Converts columns in input CSV line to columns in output CSV line."
  [csv]

  (let [[_ _ rentedatum bij-af-code bedrag _ naar-naam _ boekcode _ omschr1] csv
        bedrag (str/replace bedrag "." ",")
        result [""
                rentedatum
                (if (= "D" bij-af-code) bedrag "")
                (if (= "C" bij-af-code) bedrag "")
                boekcode
                (if-not (seq naar-naam) omschr1 naar-naam)
                (convert-description csv)]]

    (str (->> result
              (map #(str "\"" % "\""))
              (interpose ",")
              (apply str))
         "\n")))

(defn convert-line
  "Converts CSV line. Appends to output file (determined by account
  number in first column: inputfilename#accountnumber.csv)."
  [fname csv-line]
  (let [csv (get-csvs csv-line)
        rekeningnummer-rekeninghouder (first csv)
        ext (.extname path fname)
        base (.basename path fname ext)
        ofname (str base "#" rekeningnummer-rekeninghouder ext)]

    (when (and (.existsSync fs ofname)
               (not (contains? @output-fnames ofname)))
      (do (.unlinkSync fs ofname)))

    ;; If the file already existed it was deleted and if didn't exist
    ;; it was also okay. So always add it to the set of output-fnames.
    (swap! output-fnames conj ofname)

    (.appendFile fs
                 ofname
                 (convert-columns csv)
                 (fn [err] (when err (println "***ERROR***"))))
    rekeningnummer-rekeninghouder))

(defn convert
  "Converts CSV lines."
  [fname csv-str]
  (->> csv-str
       str/split-lines
       (map (partial convert-line fname))))

;; ----
;; MAIN

(let [[_ _ _ & fnames] (.-argv process)]
  (doseq [fname fnames]
    (println (str fname ":"))
    (let [lines (.readFileSync fs fname "utf8")
          accounts (distinct (convert fname lines))]

      (println (str "\t"
                    (->> accounts
                         (interpose "\n")
                         (apply str)))))))
