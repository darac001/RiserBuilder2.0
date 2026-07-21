;; ============================================================
;; Intrusion Riser Layout Engine
;; Version 0.2
;;
;; - Correct trunk topology (feeds INTO panel)
;; - Devices placed along trunk (left → right)
;; ============================================================



;; ------------------------------------------------------------
;; Insert Panel Block
;; ------------------------------------------------------------

(defun it-insert-panel (block-name insert-point) 
  (command "_-INSERT" block-name insert-point 1 1 0 "")
)



;; ------------------------------------------------------------
;; Layout Single Panel
;; ------------------------------------------------------------

(defun it-layout-panel (panel base-point cable-data / panel-type block-name) 

  (setq panel-type (nth 1 panel))

  (prompt (strcat "\nPanel Type: " panel-type))

  (setq block-name (get-it-panel-block panel-type))

  (prompt (strcat "\nBlock Name: " block-name))

  ;; insert panel
  (it-insert-panel block-name base-point)

  ;; layout home runs
  (it-layout-home-runs panel base-point cable-data)
)



;; ------------------------------------------------------------
;; Layout Home Runs 
;; ------------------------------------------------------------

(defun it-layout-home-runs (panel base-point cable-data / devices device-rows 
                            panel-left trunk-start trunk-end x y dev-block cable 
                            wire-point text-point panel-cables wire-counts wire-tag 
                            row
                           ) 

  (setq devices (get-it-home-run-devices panel))

  (setq device-rows (split-it-device-rows devices))
  (prompt "\nDEVICE ROWS:")
  (princ device-rows)

  (setq panel-cables (get-it-home-run-cables 
                       (nth 0 panel)
                       cable-data
                     )
  )


  (setq wire-counts (count-it-cables panel-cables))


  (setq wire-tag (format-it-cable-tag wire-counts))




  (setq y (- (cadr base-point) (/ *it-panel-height* 2)))

  ;; LEFT EDGE OF PANEL
  (setq panel-left (- (car base-point) (/ *it-panel-width* 2.0)))

  ; ;; TRUNK START (far left)
  ; (setq trunk-start (list
  ;                     (- panel-left
  ;                        (+ *it-device-start-offset*
  ;                           (* (- (length row) 1) *it-device-spacing*)
  ;                        )
  ;                     )
  ;                     row-y
  ;                   )
  ; )

  ; ;; TRUNK END (at panel)
  ; (setq trunk-end (list panel-left y))






  ;; DRAW EACH DEVICE ROW

  (setq row-y y)

  (foreach row device-rows 

    (setq trunk-start (list 
                        (- panel-left 
                           (+ *it-device-start-offset* 
                              (* (- (length row) 1) 
                                 *it-device-spacing*
                              )
                           )
                        )
                        row-y
                      )
    )

    (setq trunk-end (list panel-left row-y))

    (command "LINE" trunk-start trunk-end "")


    (it-draw-leader 

      (list 
        (- (car trunk-end) 0.5)
        (cadr trunk-end)
      )

      (list 
        (- (car trunk-end) 0.5)
        (+ (cadr trunk-end) 0.4)
      )

      wire-tag
    )

    ;; restart x position for every row
    (setq x (car trunk-start))

    (foreach device row 

      (setq dev-block (nth 2 device))




      ;; vertical drop
      (command "LINE" 
               (list x row-y)
               (list x (- row-y *it-device-drop*))
               ""
      )



      ;; insert device
      (it-insert-device 
        dev-block
        (list x (- row-y *it-device-drop*))
      )

      (it-place-device-id 
        device
        (list x (- row-y *it-device-drop*))
      )

      ;; get cable
      (setq cable (get-it-device-cable 
                    (nth 0 panel)
                    (nth 0 device)
                    cable-data
                  )
      )


      ;; leader halfway down drop
      (setq wire-point (list 
                         x
                         (- row-y (/ *it-device-drop* 2.0))
                       )
      )


      (setq text-point (list 
                         (+ x *it-wire-tag-offset*)
                         (- row-y (/ *it-device-drop* 2.0))
                       )
      )


      (it-draw-leader 
        wire-point
        text-point
        cable
      )


      ;; move RIGHT toward panel
      (setq x (+ x *it-device-spacing*))
    ) ;; end foreach device


    ;; move down after completing one row
    (setq row-y (- row-y *it-row-spacing*))
  ) ;; end foreach row
) ;; end function

(defun it-insert-device (block-name insert-point) 
  (command "_-INSERT" block-name insert-point 1 1 0 "")
)
(defun it-place-device-id (device insert-point / label pt) 

  ;; assume device structure: (... ID TYPE BLOCK ...)
  (setq label (nth 0 device))

  (setq pt (list 
             (+ (car insert-point) *it-device-id-x-offset*)
             (+ (cadr insert-point) *it-device-id-offset*)
           )
  )

  (command "TEXT" pt *it-device-id-text-height* 0 label)
)
(defun it-draw-leader (wire-point text-point text) 

  (command 
    "_MLEADER"
    wire-point
    text-point
    (strcat text "")
    ""
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

  (setq cable-data (build-it-cable-model *it-input-data*))
  ;; Draw panels
  (foreach panel system-data 

    (it-layout-panel 
      panel
      (list 0 y)
      cable-data
    )

    (setq y (- y *it-panel-spacing*))
  )

  ;; Restore osnap
  (setvar "OSMODE" old-osnap)

  (prompt "\nIntrusion riser complete.")
  (princ)
)