(defun c:TEST-IT-PARSER (/ filename raw parsed)

  (prompt "\n--- Intrusion Parser Test ---")


  ;; Change this to your actual CSV location
  (setq filename "C:/Users/darko/Desktop/Athabasca/COMP495/Project2.0/Intrusion/it_input.csv")


  ;; Test reading CSV
  (setq raw
        (read-csv filename)
  )


  (prompt
    (strcat
      "\nRaw lines loaded: "
      (itoa (length raw))
    )
  )


  ;; Test parsing CSV
  (setq parsed
        (parse-csv raw)
  )


  (prompt
    (strcat
      "\nParsed rows: "
      (itoa (length parsed))
    )
  )


  ;; Store result for inspection
  (setq *test-it-data* parsed)


  ;; Print first row
  (prompt "\n\nFirst row:")
  (princ (car parsed))


  ;; Print last row
  (prompt "\n\nLast row:")
  (princ (car (last parsed)))


  (prompt "\n\nTest complete.")

  (princ)

)