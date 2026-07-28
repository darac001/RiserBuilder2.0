(defun c:TEST-IT-INPUT (/ ok firstRow fieldCount)

  (prompt "\n=== Testing Input Parser ===")

  ;; Load input
  (it-load-input)

  (setq ok T)

  ;; 1. Check data exists
  (if (not *it-input-data*)
    (progn
      (prompt "\nFAIL: No parsed data found.")
      (setq ok nil)
    )
  )

  ;; 2. Check first row structure
  (if ok
    (progn
      (setq firstRow (car *it-input-data*))

      (if (not (listp firstRow))
        (progn
          (prompt "\nFAIL: First row is not a list.")
          (setq ok nil)
        )
      )
    )
  )

  ;; 3. Check field count consistency
  (if ok
    (progn
      (setq fieldCount (length firstRow))

      (foreach row *it-input-data*
        (if (/= (length row) fieldCount)
          (progn
            (prompt "\nFAIL: Inconsistent field counts detected.")
            (setq ok nil)
          )
        )
      )
    )
  )

  ;; 4. Print sample row for verification
  (if ok
    (progn
      (prompt "\n\nSample Row (First Row):")
      (princ firstRow)
    )
  )

  ;; 5. Final result
  (if ok
    (prompt "\n\nPASS: Input parser working correctly.")
  )

  (princ)
)


(defun c:TEST-IT-DATA-MODEL (/ system-data cable-data)

  (prompt "\n--- Testing Intrusion Data Model ---")


  ;; Build panel/device data model
  (setq system-data 
        (build-it-data-model *it-input-data*)
  )


  ;; Display first panel object
  (prompt "\n\nFirst Panel Object:")
  (princ (car system-data))


  ;; Build cable model
  (setq cable-data 
        (build-it-cable-model *it-input-data*)
  )


  ;; Display first cable entry
  (prompt "\n\nFirst Cable Entry:")
  (princ (car cable-data))


  (prompt "\n\nData model test complete.")

  (princ)
)



(defun c:TEST-IT-LAYOUT (/ panels cables panel height)

  (prompt "\n--- Testing Intrusion Layout Engine ---")


  ;; Build data
  (setq panels 
        (build-it-data-model *it-input-data*)
  )

  (setq cables 
        (build-it-cable-model *it-input-data*)
  )


  ;; Test first panel
  (setq panel (car panels))


  ;; Test layout calculation
  (setq height 
        (it-get-panel-layout-height panel)
  )


  (prompt
    (strcat
      "\nPanel: "
      (nth 0 panel)
    )
  )


  (prompt
    (strcat
      "\nCalculated Layout Height: "
      (rtos height 2 2)
    )
  )


  ;; Test device grouping

  (prompt "\n\nHome Run Devices:")

  (princ
    (get-it-home-run-devices panel)
  )


  (prompt "\n\nDaisy Devices:")

  (princ
    (get-it-daisy-devices panel)
  )


  (prompt "\n\nLayout engine unit test complete.")

  (princ)
)