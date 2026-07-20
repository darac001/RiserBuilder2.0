;;; ============================================================
;;; Intrusion CSV Input Parser
;;;
;;; Reads intrusion project CSV files and converts them into
;;; AutoLISP lists used by the Intrusion Riser Builder data model.
;;;
;;; ============================================================



;; Reads CSV file and returns list of raw lines.
;;
;; Input:
;;   filename - Full path to CSV file
;;
;; Returns:
;;   List of CSV rows as strings

(defun read-csv (filename / file line data) 

  (setq file (open filename "r"))
  (setq data '())

  (while (setq line (read-line file)) 

    (setq data (cons line data))
  )

  (close file)

  ;; cons adds elements to beginning,
  ;; reverse restores original order
  (reverse data)
)



;; Parses a single CSV row while preserving commas inside quotes.
;;
;; Example:
;; "Door Contact, Recessed" remains one field.

(defun parseCSV (str / i ch inQuotes token result) 

  (setq i        1
        inQuotes nil
        token    ""
        result   '()
  )

  (while (<= i (strlen str)) 

    (setq ch (substr str i 1))

    (cond 

      ((= ch "\"")

       (setq inQuotes (not inQuotes))
      )


      ((and (= ch ",") (not inQuotes))

       (setq result (cons token result))

       (setq token "")
      )


      (T

       (setq token (strcat token ch))
      )
    )

    (setq i (1+ i))
  )


  (setq result (cons token result))

  (reverse result)
)



;; Removes quotation marks from CSV fields.
;;
;; Input:
;;   s - CSV field value
;;
;; Returns:
;;   Field without surrounding quotes

(defun stripQuotes (s) 

  (if 

    (and 
      (> (strlen s) 1)
      (= (substr s 1 1) "\"")
      (= (substr s (strlen s) 1) "\"")
    )

    (substr s 2 (- (strlen s) 2))

    s
  )
)



;; Converts raw CSV lines into structured data rows.
;;
;; Removes header row and strips quotation marks.
;;
;; Input:
;;   lines - List of CSV lines
;;
;; Returns:
;;   Parsed CSV data list

(defun parse-csv (lines / rows) 

  ;; remove header
  (setq lines (cdr lines))


  (foreach line lines 

    (setq rows (cons 
                 (mapcar 'stripQuotes (parseCSV line))
                 rows
               )
    )
  )


  (reverse rows)
)



;;; ============================================================
;;; Intrusion Input Storage
;;;
;;; *rb-input-lines* - Raw CSV lines
;;; *rb-input-data*  - Parsed CSV data
;;;
;;; ============================================================

(setq *it-input-lines* nil)
(setq *it-input-data* nil)



;;; ============================================================
;;; Loads intrusion input CSV
;;;
;;; Uses:
;;;   it-get-input-file
;;;   read-csv
;;;   parse-csv
;;;
;;; Updates:
;;;   *rb-input-lines*
;;;   *rb-input-data*
;;;
;;; ============================================================

(defun it-load-input (/ filename) 

  (setq filename "C:/Users/darko/Desktop/Athabasca/COMP495/Project2.0/Intrusion/it_input.csv")


  (setq *it-input-lines* (read-csv filename))


  (setq *it-input-data* (parse-csv *it-input-lines*))


  (prompt "\nIntrusion input CSV loaded.")

  (princ)
)

(defun c:TEST-IT-LOAD () 

  (prompt "\n--- Testing Intrusion Input Load ---")

  (it-load-input)


  (prompt "\n\nLast row:")

  (princ 
    (car (last *it-input-data*))
  )


  (prompt "\n\nLoad test complete.")

  (princ)
)