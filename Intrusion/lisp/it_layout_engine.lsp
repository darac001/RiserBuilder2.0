;; ============================================================
;; Intrusion Riser Layout Engine
;;
;; Version 0.1
;;
;; Currently:
;; - Panel placement only
;;
;; ============================================================



;; ------------------------------------------------------------
;; Insert Panel Block
;; ------------------------------------------------------------

(defun it-insert-panel (block-name insert-point) 

  (command "_-INSERT" block-name insert-point 1 1 0 "")
)



;; ------------------------------------------------------------
;; Layout Single Panel
;;
;; Panel structure:
;;
;; (
;;  Panel ID
;;  Panel Type
;;  Panel Block
;;  Devices
;; )
;;
;; ------------------------------------------------------------

(defun it-layout-panel (panel base-point) 

  (setq panel-type (nth 1 panel))

  (prompt (strcat "\nPanel Type: " panel-type))

  (setq block-name (get-it-panel-block panel-type))

  (prompt (strcat "\nBlock Name: " block-name))

  (it-insert-panel 
    block-name
    base-point
  )
)



;; ------------------------------------------------------------
;; Draw Complete Intrusion Riser
;; ------------------------------------------------------------

(defun IT-DRAW-RISER (/ system-data y old-osnap) 

  (prompt "\n--- Drawing Intrusion Riser ---")


  ;; Load libraries
  (it-load-device-library)
  (it-load-panel-library)


  ;; Load input if needed
  (if (not *it-input-data*) 
    (it-load-input)
  )


  ;; Build model
  (setq system-data (build-it-data-model *it-input-data*))


  ;; Disable osnap
  (setq old-osnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)


  ;; Starting Y position
  (setq y 0)


  ;; Draw panels
  (foreach panel system-data 

    (it-layout-panel 
      panel
      (list 0 y)
    )

    (setq y (- y *it-panel-spacing*))
  )


  ;; Restore osnap
  (setvar "OSMODE" old-osnap)


  (prompt "\nIntrusion riser complete.")

  (princ)
)