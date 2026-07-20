;; ============================================================
;; Intrusion Data Model Helper Functions
;; ============================================================


;; Returns only home run devices from a panel

(defun get-it-home-run-devices (panel / result device) 


  (setq result '())


  (foreach device (nth 3 panel) 

    (if (= (nth 4 device) "Home_Run") 

      (setq result (cons device result))
    )
  )


  (reverse result)
)



;; Returns only daisy chain devices from a panel

(defun get-it-daisy-chain-devices (panel / result device) 


  (setq result '())


  (foreach device (nth 3 panel) 

    (if (= (nth 4 device) "Daisy_Chain") 

      (setq result (cons device result))
    )
  )


  (reverse result)
)


(defun c:TEST-IT-LOOPS (/ panel) 


  (prompt "\n--- Testing Loop Separation ---")


  (setq panel (car *test-it-model*))


  (prompt "\n\nHome Run Devices:")
  (princ 
    (get-it-home-run-devices panel)
  )


  (prompt "\n\nDaisy Chain Devices:")
  (princ 
    (get-it-daisy-chain-devices panel)
  )


  (princ)
)