;; ============================================================
;; Intrusion Data Model Helper Functions
;;
;; Purpose:
;;   Provides filtering and grouping functions used by the
;;   intrusion riser layout engine.
;;
;; Functions:
;;   - Filter devices by loop type
;;   - Split devices into drawing rows
;;   - Calculate required panel height
;;   - Group daisy-chain devices by loop number
;;   - Retrieve keypad devices
;;
;; ============================================================


;; Return home-run devices from panel

(defun get-it-home-run-devices (panel / result device) 
  (setq result '())
  (foreach device (nth 3 panel) 
    (if (= (nth 4 device) "Home_Run") 
      (setq result (cons device result))
    )
  )
  (reverse result)
)



;; Return daisy-chain devices from panel

; (defun get-it-daisy-chain-devices (panel / result device) 
;   (setq result '())
;   (foreach device (nth 3 panel) 
;     (if (= (nth 4 device) "Daisy_Chain") 
;       (setq result (cons device result))
;     )
;   )
;   (reverse result)
; )


;; Split devices into drawing rows based on device limit
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

(defun it-get-panel-layout-height (panel / home-devices home-rows daisy-devices 
                                   daisy-loops total-rows spacing
                                  ) 

  ;; Home runs
  (setq home-devices (get-it-home-run-devices panel))
  (setq home-rows (if home-devices 
                    (length (split-it-device-rows home-devices))
                    0
                  )
  )

  ;; Daisy chains
  (setq daisy-devices (get-it-daisy-devices panel))
  (setq daisy-loops (if daisy-devices 
                      (length (get-it-daisy-loops daisy-devices))
                      0
                    )
  )

  ;; Total rows
  (setq total-rows (+ home-rows daisy-loops))

  ;; Spacing calculation (current logic)
  (setq spacing (* (max 0 (- total-rows 1)) *it-row-spacing*))

  ;; DEBUG LOGGING
  ; (it-debug-log "----------------------------")
  ; (it-debug-log (strcat "Panel: " (vl-princ-to-string panel)))
  ; (it-debug-log (strcat "Home rows: " (itoa home-rows)))
  ; (it-debug-log (strcat "Daisy loops: " (itoa daisy-loops)))
  ; (it-debug-log (strcat "Total rows: " (itoa total-rows)))
  ; (it-debug-log (strcat "Row spacing: " (rtos *it-row-spacing* 2 3)))
  ; (it-debug-log (strcat "Computed spacing: " (rtos spacing 2 3)))

  ;; Final height
  (+ 
    *it-panel-height*
    *it-device-drop*
    spacing
  )
)


(defun get-it-daisy-devices (panel / devices result device) 
  (setq devices (nth 3 panel))
  (setq result '())

  (foreach device devices 
    (if (= "Daisy_Chain" (nth 4 device)) 
      (setq result (cons device result))
    )
  )


  (reverse result)
)

;; Split devices into drawing rows based on device limit
(defun get-it-daisy-loops (panel / devices loops device loop-no existing) 

  (setq devices (get-it-daisy-devices panel))
  (setq loops '())
  
  (foreach device devices 

    (setq loop-no (nth 5 device))
    (setq existing (assoc loop-no loops))

    (if existing 
      ;; add device to existing loop
      (setq loops (subst 
                    (cons loop-no 
                          (append (cdr existing) 
                                  (list device)
                          )
                    )
                    existing
                    loops
                  )
      )
      ;; create new loop
      (setq loops (cons 
                    (cons loop-no 
                          (list device)
                    )
                    loops
                  )
      )
    )
  )
  (reverse loops)
)


;; Get Daisy Chain Cable Type (needs only 1st device)
;;
;; Input:
;;   loop-devices
;;
;; Example:
;; (
;;  ("DC#01" "DC" "BLOCK" "B" "Daisy_Chain" 1)
;;  ("DC#02" "DC" "BLOCK" "B" "Daisy_Chain" 1)
;; )
;;
;; Returns:
;;   "B"


(defun get-it-daisy-cable (loop-devices / device) 

  (setq device (car loop-devices))

  (if device 

    (nth 3 device)

    nil
  )
)

;; Get Daisy Chain Loops
;;
;; Input:
;;   devices
;;
;; Returns:
;; (
;;   (loop-no device device device)
;;   (loop-no device device)
;; )


(defun get-it-daisy-loops (devices / loops device loop-no existing) 

  (setq loops '())
  (foreach device devices 
    ;; only Daisy Chain devices

    (if (= (nth 4 device) "Daisy_Chain") 

      (progn 

        ;; loop number
        (setq loop-no (nth 5 device))

        ;; check if loop already exists

        (setq existing (assoc loop-no loops))

        (if existing 

          ;; add device to existing loop

          (setq loops (subst 
                        (append existing (list device))
                        existing
                        loops
                      )
          )

          ;; create new loop

          (setq loops (cons 
                        (list loop-no device)
                        loops
                      )
          )
        )
      )
    )
  )
  ;; preserve order
  (reverse loops)
)


;; Return keypad devices from panel
(defun get-it-keypad-devices (panel / devices) 
  (setq devices (nth 3 panel)) ;;

  (vl-remove-if-not 
    '(lambda (d) 
       (= (strcase (nth 1 d)) "KP") ;;
     )
    devices
  )
)


