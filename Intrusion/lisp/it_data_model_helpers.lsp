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



(defun split-it-device-rows (devices / rows row count) 

  (setq rows '())
  (setq row '())
  (setq count 0)

  (foreach device devices 

    (setq row (append row (list device)))
    (setq count (+ count 1))

    ;; reached row limit
    (if (= count *it-device-limit*) 

      (progn 
        (setq rows (append rows (list row)))
        (setq row '())
        (setq count 0)
      )
    )
  )

  ;; add remaining devices
  (if row 
    (setq rows (append rows (list row)))
  )

  rows
)

;; ============================================================
;; Calculate panel layout height
;;
;; Determines vertical space needed for a panel based on:
;; - number of home run device rows
;; - row spacing
;; - device drop
;; - panel height
;;
;; ============================================================

(defun it-get-panel-layout-height (panel / devices device-rows)

  (setq devices
        (get-it-home-run-devices panel)
  )

  (setq device-rows
        (split-it-device-rows devices)
  )

  (prompt "\nDevices: ")
  (princ (length devices))

  (prompt "\nRows: ")
  (princ (length device-rows))

  (prompt "\nPanel height: ")
  (princ *it-panel-height*)

  (prompt "\nDevice drop: ")
  (princ *it-device-drop*)

  (prompt "\nRow spacing: ")
  (princ *it-row-spacing*)

  ;; temporary calculation without panel-gap
  (+ 
    *it-panel-height*
    *it-device-drop*
    (* 
      (- (length device-rows) 1)
      *it-row-spacing*
    )
  )
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


(defun c:TEST-IT-PANEL-HEIGHT (/ model panel)

  (setq model
        (build-it-data-model *it-input-data*)
  )

  (foreach panel model

    (prompt "\nPanel: ")
    (princ (nth 0 panel))

    (prompt "\nRows: ")
    (princ 
      (length 
        (split-it-device-rows 
          (get-it-home-run-devices panel)
        )
      )
    )

    (prompt "\nHeight: ")
    (princ
      (it-get-panel-layout-height panel)
    )
  )

  (princ)
)