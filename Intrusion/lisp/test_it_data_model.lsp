(defun c:TEST-IT-MODEL (/ model)


  (prompt "\n--- Testing Intrusion Data Model ---")


  ;; Build model from parser output

  (setq model
    (build-it-data-model
      *test-it-data*
    )
  )


  ;; store for inspection

  (setq *test-it-model* model)


  (prompt "\n\nGenerated Model:")

  (princ model)


  (prompt "\n\nTest complete.")

  (princ)

)