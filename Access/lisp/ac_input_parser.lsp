;;; ============================================================
;;; Access Control CSV Input Parser
;;;
;;; Reads project CSV files and converts them into AutoLISP
;;; lists used by the Riser Builder data model.
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
  (setq data '()) ; empty list

  (while (setq line (read-line file)) 
    (setq data (cons line data))
  )

  (close file)
  (reverse data) ;cons adds elements to beginning of the list so we need to reverse it
)



;; Parses a single CSV row while preserving commas inside quotes.
;;
;; Example:
;; "CR,DC,ES,RX" remains one field.

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






;;; Removes quotation marks from CSV fields.
;;;
;;; Input:
;;;   s - CSV field value
;;;
;;; Returns:
;;;   Field without surrounding quotes
(defun stripQuotes (s) 
  (if 
    (and (> (strlen s) 1) 
         (= (substr s 1 1) "\"")
         (= (substr s (strlen s) 1) "\"")
    )
    (substr s 2 (- (strlen s) 2))
    s
  )
)


;;; Converts raw CSV lines into structured data rows.
;;;
;;; Removes the header row and strips quotation marks
;;; from each field.
;;;
;;; Input:
;;;   lines - List of CSV lines
;;;
;;; Returns:
;;;   Parsed CSV data list
(defun parse-csv (lines / header rows) 

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

  ;;; Stores currently loaded CSV input data.
  ;;;
  ;;; *rb-input-lines* - Raw CSV lines
  ;;; *rb-input-data*  - Parsed CSV data

  (setq *rb-input-lines* nil)
  (setq *rb-input-data* nil)


  ;;; Loads project input CSV and stores parsed data.
  ;;;
  ;;; Uses:
  ;;;   rb-get-input-file
  ;;;   read-csv
  ;;;   parse-csv
  ;;;
  ;;; Updates:
  ;;;   *rb-input-lines*
  ;;;   *rb-input-data*
  (defun rb-load-input () 

    (setq *rb-input-lines* (read-csv (rb-get-input-file)))

    (setq *rb-input-data* (parse-csv *rb-input-lines*))

    (prompt "\nInput CSV loaded.")
    (princ)
  )