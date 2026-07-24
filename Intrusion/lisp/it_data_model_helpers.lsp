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

  (setq devices (get-it-home-run-devices panel))

  (setq device-rows (split-it-device-rows devices))

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

;; ------------------------------------------------------------
;; Get Daisy Chain Cable Type
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
;;
;; ------------------------------------------------------------

(defun get-it-daisy-cable (loop-devices / device) 

  (setq device (car loop-devices))

  (if device 

    (nth 3 device)

    nil
  )
)

;; ------------------------------------------------------------
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
;;
;; Example:
;;
;; (
;;   (1 DC#01 DC#02 DC#03)
;;   (2 DC#04 DC#05)
;; )
;;
;; ------------------------------------------------------------

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

