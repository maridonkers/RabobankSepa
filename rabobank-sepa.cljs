#!/usr/bin/env lumo
;;
;; Converts Rabobank 2018 CSV-file format (as exported by Rabobank
;; internet banking) to an KMyMoney importable format.
;;
;; https://www.rabobank.nl/images/formaatbeschrijving-csv-extensie_29933458.pdf
;; https://www.rabobank.nl/images/transactiesoortcodes_29842987.pdf
;;
;; Version 0.3.0
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
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; ---------------
;; NODE.JS INTEROP

(def fs (js/require "fs"))
(def path (js/require "path"))
(def process (js/require "process"))

;; -------------
;; RABOBANK SEPA

(def DATE-REGEXP #"[0-9]{4}-[0-9]{2}-[0-9]{2}")
;;TODO check/complete these.
(def BBAN-REGEXP #"(?i)P?[0-9]+")
(def IBAN-REGEXP #"(?i)[A-Z]{2}[0-9]{2}[A-Z0-9]{4,}")
(def AMOUNT-REGEXP #"[+-][0-9]+,?[0-9]*")

(s/def ::iban-bban (s/and string?
                          #(<= (count %) 34)
                          (s/or :empty #(= (count %) 0)
                                :bban #(re-matches BBAN-REGEXP %)
                                :iban #(re-matches IBAN-REGEXP %))))

(s/def ::muntsoort (s/and string? #(<= (count %) 4)))

(s/def ::bic (s/and string? #(<= (count %) 11)))

(s/def ::volgnr (s/and string? #(<= (count %) 18)))

(s/def ::datum (s/and string?
                      #(<= (count %) 10)
                      (s/or :empty #(= (count %) 0)
                            :date #(re-matches DATE-REGEXP %))))

(s/def ::rentedatum ::datum)

;; (s/def ::bij-af-code (s/and string? #(= (count %) 1) #(re-matches #"(?i)[CD]{1}" %)))

(s/def ::bedrag (s/and string? #(<= (count %) 18) #(re-matches AMOUNT-REGEXP %)))

(s/def ::saldo ::bedrag)

(s/def ::naam (s/and string? #(<= (count %) 70)))

;;TODO Check why this BIC is 15 instead of 11 chars.
(s/def ::bic-tegenpartij (s/and string? #(<= (count %) 15)))

(s/def ::code (s/and string? #(<= (count %) 4)))

(s/def ::batch-id (s/and string? #(<= (count %) 35)))

(s/def ::transactiereferentie (s/and string? #(<= (count %) 35)))

(s/def ::machtigingskenmerk (s/and string? #(<= (count %) 35)))

(s/def ::incassant-id (s/and string? #(<= (count %) 35)))

(s/def ::betalingskenmerk (s/and string? #(<= (count %) 35)))

;; (s/def ::filler (s/and string? #(<= (count %) 6)))

(s/def ::omschrijving (s/and string? #(<= (count %) 140)))

(s/def ::reden-retour (s/and string? #(<= (count %) 75)))

;;TODO Check this one (it's numeric without +/- ?)
(s/def ::oorspr-bedrag (s/and string? #(<= (count %) 18)))

;;TODO Check this (spec says 11 chars but ::muntsoort only has 4 chars?)
(s/def ::oorspr-munt (s/and string? #(<= (count %) 11)))

;;TODO Check this one (it's numeric without +/- ?)
(s/def ::koers (s/and string? #(<= (count %) 11)))

;; (s/def ::end-to-end-id ::comment-field)
;; (s/def ::id-tegenrekeninghouder ::comment-field)
;; (s/def ::mandaat-id ::comment-field)

;; NIEUW FORMAAT
;;
;; "IBAN/BBAN","Munt","BIC","Volgnr","Datum","Rentedatum","Bedrag","Saldo na trn","Tegenrekening IBAN/BBAN","Naam tegenpartij","Naam uiteindelijke partij","Naam initiërende partij","BIC tegenpartij","Code","Batch ID","Transactiereferentie","Machtigingskenmerk","Incassant ID","Betalingskenmerk","Omschrijving-1","Omschrijving-2","Omschrijving-3","Reden retour","Oorspr bedrag","Oorspr munt","Koers"

;; Veld	Omschrijving			Type		Lengte	Inhoud/Toelichting
(s/def ::sepa-columns (s/cat
;; 1	IBAN_BBAN	                Alfanumeriek	34	Eigen Rekeningnummer
   :1 ::iban-bban
;; 2	MUNTSOORT			Alfanumeriek	4	Muntsoort eigen rekening
   :2 ::muntsoort
;; 3	BIC			        Alfanumeriek	11	BIC eigen rekening
   :3 ::bic
;; 4	volgnr			        Alfanumeriek	18	Volgnummer
   :4 ::volgnr
;; 5	DATUM		  	        Datum           10	Verwerkings- of boekdatum. Formaat: EEJJ-MM-DD; Bijvoorbeeld 2017-07-31
   :5 ::datum
;; 6	RENTEDATUM			Datum	        10	(Rente|Valuta)datum. Formaat: EEJJ-MM-DD; Bijvoorbeeld 2017-07-31
   :6 ::rentedatum
;; 7	BEDRAG				Numeriek	18	Prefix +/-; ISO4217 decimalen. Decimalen worden weergegeven met een comma
   :7 ::bedrag
;; 8	SALDO_NA_TRANSACTIE		Numeriek	18	Prefix +/-; ISO4217 decimalen. Decimalen worden weergegeven met een comma
   :8 ::saldo
;; 9	TEGENREKENING_IBAN_BBAN		Alfanumeriek	34	Tegenrekeningnummer
   :9 ::iban-bban
;; 10	NAAM_TEGENPARTIJ		Alfanumeriek	70	Naam van de tegenpartij
   :10 ::naam
;; 11	NAAM_UITEINDELIJKE_PARTIJ	Alfanumeriek	70	Naam van de uiteindelijke partij
   :11 ::naam
;; 12	NAAM_INITIERENDE_PARTIJ	        Alfanumeriek	70	Naam van de initiërende partij
   :12 ::naam
;; 13	BIC_TEGENPARTIJ			Alfanumeriek	15	BIC rekening tegenpartij
   :13 ::bic-tegenpartij
;; 14	CODE			        Alfanumeriek	4	Transactiecode
   :14 ::code
;; 15	BATCH_ID			Alfanumeriek	35
   :15 ::batch-id
;; 16	TRANSACTIEREFERENTIE		Alfanumeriek	35
   :16 ::transactiereferentie
;; 17	MACHTIGINGSKENMERK		Alfanumeriek	35
   :17 ::machtigingskenmerk
;; 18	INCASSANT_ID		        Alfanumeriek	35
   :18 ::incassant-id
;; 19	BETALINGSKENMERK		Alfanumeriek	35
   :19 ::betalingskenmerk
;; 20	OMSCHRIJVING-1		        Alfanumeriek	140
   :20 ::omschrijving
;; 21	OMSCHRIJVING-2			Alfanumeriek	140
   :21 ::omschrijving
;; 22	OMSCHRIJVING-3			Alfanumeriek	140
   :22 ::omschrijving
;; 23	REDEN_RETOUR		        Alfanumeriek	75
   :23 ::reden-retour
;; 24	OORSPR_BEDRAG		        Alfanumeriek	18
   :24 ::oorspr-bedrag
;; 25	OORSPR_MUNT		        Alfanumeriek	11
   :25 ::oorspr-munt
;; 26	KOERS		                Alfanumeriek	11
   :26 ::koers
   
;; 4	BY_AF_CODE			Alfanumeriek	1	D of C
   ;; :4 ::bij-af-code

;; 8	BOEKDATUM			Numeriek	8	Formaat: EEJJMMDD
   ;; :8 ::boekdatum
;; 10	FILLER				Alfanumeriek	6	
   ;; :10 ::filler
;; 11	OMSCHR1				Alfanumeriek	35	
   ;; :11 ::omschr
;; 12	OMSCHR2				Alfanumeriek	35	
   ;; :12 ::omschr
;; 13	OMSCHR3				Alfanumeriek	35	
   ;; :13 ::omschr
;; 14	OMSCHR4				Alfanumeriek	35	
   ;; :14 ::omschr
;; 15	OMSCHR5				Alfanumeriek	35	
   ;; :15 ::omschr
;; 16	OMSCHR6				Alfanumeriek	35	
   ;; :16 ::omschr
;; 17	END_TO_END_ID			Alfanumeriek	35	SEPA Credit Transfer: Kenmerk opgegeven door de opdrachtgever
   ;; :17 ::end-to-end-id
;; 18	ID_TEGENREKENINGHOUDER		Alfanumeriek	35	SEPA Credit Transfer: Identificatie van de tegenrekeninghouder
   ;; :18 ::id-tegenrekeninghouder
;; 19	MANDAAT_ID			Alfanumeriek	35	SEPA Direct Debet: Kenmerk machtiging
   ;; :19 ::mandaat-id
))

;; ---------
;; KYMYMONEY
;;
;; Beschrijving uitvoerformaat (which is a KMyMoney compatible import formaat).
;;
;; Veld	Omschrijving			Type
;; 1	NUMBER				Alfanumeriek
;; 2	DATE				Numeriek
;; 3	DEBIT/CREDIT			Numeriek
;; 4	CATEGORY (code)			Alfanumeriek
;; 5	PAYEE (tegenrekening)		Alfanumeriek
;; 6	MEMO (concatenated fields)	Alfanumeriek
;;
;; The MEMO field in the output is a concatenation of various fields
;; taken from the SEPA input. Spaces are inserted where required, (to
;; accommodate readability.)
;;

;; Set with output filenames, which is used to delete existing files
;; only once.
(def output-fnames (atom #{}))

(defn get-csv-columns
  "Gets CSV columns as vector. The enclosing quotes are
  removed. Nested quotes are not allowed in Rabobank SEPA."
  [csv-line]
  (let [columns (map second (re-seq #"\"([^\"]*)\"" csv-line))]
    
    (when-not (s/valid? ::sepa-columns columns)
      (println (str "\t" (s/explain ::sepa-columns columns))))
    columns))

(defn convert-description
  "Converts description."
  [cvs]

  (let [[_ _ _ _ _ _ _ _
         tegenrekening
         _ _ _ _ _ _ _
         machtigings-kenmerk incassant-id
         betalingskenmerk
         omschrijving-1 omschrijving-2 omschrijving-3
         _ _ _ _] cvs

        omschrijving (->>
                      (str omschrijving-1 omschrijving-2 omschrijving-3)
                      str/trim)

        extra (->> [machtigings-kenmerk incassant-id]
                   (map str/trim)
                   (interpose " ")
                   (filter #(seq %))
                   (apply str)
                   str/trim)]

    (str (when (seq tegenrekening) (str "[" tegenrekening "] "))
         (str (when (seq betalingskenmerk) (str betalingskenmerk " "))
              omschrijving
              (when (seq extra) (str " " extra))))))

(defn convert-columns
  "Converts columns in input CSV line to columns in output CSV line."
  [csv]

  (let [[_ _ _
         volgnr datum
         _
         bedrag
         _ _
         naam-tegenpartij
         _ _ _
         code
         _ _ _ _ _ _ _ _ _ _ _ _] csv
        
        result [volgnr
                datum
                bedrag
                code
                naam-tegenpartij
                (convert-description csv)]]

    (str (->> result
              (map #(str "\"" (if (empty? %) " " %) "\""))
              (interpose ",")
              (apply str))
         "\n")))

(defn convert-line
  "Converts CSV line. Appends to output file (determined by account
  number in first input file column: inputfilename#accountnumber.csv)."
  [fname csv-line]
  (let [csv (get-csv-columns csv-line)
        iban-bban (first csv)
        ext (path.extname fname)
        base (path.basename fname ext)
        ofname (str base "#" iban-bban ext)]

    (when (and (fs.existsSync ofname)
               (not (contains? @output-fnames ofname)))
      (do (fs.unlinkSync ofname)))

    ;; If the file already existed it was deleted and if didn't exist
    ;; it was also okay. So always add it to the set of output-fnames.
    (swap! output-fnames conj ofname)

    (fs.appendFileSync ofname
                       (convert-columns csv)
                       (fn [err] (when err (println "***ERROR***"))))

    iban-bban))

(defn convert
  "Converts CSV lines."
  [fname csv-str]
  (->> csv-str
       str/split-lines
       rest
       (map (partial convert-line fname))))

;; ----
;; MAIN

(let [[_ _ _ & fnames] (.-argv process)]
  (doseq [fname fnames]
    (println (str fname ":"))
    (let [lines (fs.readFileSync fname "utf8")
          accounts (distinct (convert fname lines))]

      (println (str "\t"
                    (->> accounts
                         (interpose "\n")
                         (apply str)))))))
