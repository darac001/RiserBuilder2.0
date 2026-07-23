;; ============================================================
;; Intrusion Riser Layout Engine
;; Version 0.2
;;
;; - Correct trunk topology (feeds INTO panel)
;; - Devices placed along trunk (left → right)
;; ============================================================



;; Layout State
;; Keeps track of current riser position

(defun it-create-layout-state (base-point) 
  (list 
    (cadr base-point) ;; current y
    0 ;; row index
  )
)
(defun it-get-state-y (state) 
  (nth 0 state)
)
(defun it-get-state-row (state) 
  (nth 1 state)
)
(defun it-update-layout-state (state y) 
  (list 
    y
    (1+ (nth 1 state))
  )
)


;; ------------------------------------------------------------
;; Insert Panel Block
;; ------------------------------------------------------------

(defun it-insert-panel (block-name insert-point / ent) 
  (rb-set-layer *it-layer-device*)
  (command "_-INSERT" block-name insert-point 1 1 0 "")

  (setq ent (entlast))

  ent
)

(defun it-insert-psu (block-name insert-point) 

  (rb-set-layer *it-layer-device*)

  (command "_-INSERT" block-name insert-point 1 1 0 "")
)

(defun rb-set-layer (layer-name) 
  (if (not (tblsearch "LAYER" layer-name)) 
    (command "-LAYER" "M" layer-name "")
  )
  (setvar "CLAYER" layer-name)
)


(defun get-it-daisy-right-point (insert-point) 

  (list 
    (+ (car insert-point) 
       *it-device-width*
    )

    (cadr insert-point)
  )
)


;; ------------------------------------------------------------
;; Layout Single Panel
;; ------------------------------------------------------------

(defun it-layout-panel (panel base-point cable-data / panel-type block-name psu-block 
                        psu-point panel-entity home-run-devices home-run-rows daisy-y
                       ) 

  (setq panel-type (nth 1 panel))

  (prompt (strcat "\nPanel Type: " panel-type))

  (setq block-name (get-it-panel-block panel-type))

  (prompt (strcat "\nBlock Name: " block-name))

  ;; insert panel
  (setq panel-entity (it-insert-panel block-name base-point))

  ;; Insert external PSU if required

  (if (it-panel-requires-psu panel-type) 

    (progn 

      (setq psu-block (get-it-panel-ps-block panel-type))

      (setq psu-point (list 
                        (+ (car base-point) 
                           *it-panel-width*
                           1.0
                        )
                        (cadr base-point)
                      )
      )

      (it-insert-psu psu-block psu-point)
    )
  )

  ;; write panel ID attribute
  (it-set-attribute 
    panel-entity
    "PANEL_ID"
    (nth 0 panel)
  )

  ;; layout home runs
  (it-layout-home-runs panel base-point cable-data)


  (setq home-run-devices (get-it-home-run-devices panel))

  (setq home-run-rows (length 
                        (split-it-device-rows home-run-devices)
                      )
  )


  (setq daisy-y (- 
                  (cadr base-point)

                  (/ *it-panel-height* 2.0)

                  (* home-run-rows *it-row-spacing*)
                )
  )

  ;; layout daisy loops
  (it-layout-daisy-loops 

    panel

    (list 

      (- 
        (car base-point)

        (/ *it-panel-width* 2.0)
      )

      daisy-y
    )
  )
)



;; ------------------------------------------------------------
;; Layout Home Runs 
;; ------------------------------------------------------------

(defun it-layout-home-runs (panel base-point cable-data / devices device-rows 
                            panel-left trunk-start trunk-end x y row-y dev-block cable 
                            wire-point text-point row-cables wire-counts wire-tag row
                           ) 

  (setq devices (get-it-home-run-devices panel))

  (setq device-rows (split-it-device-rows devices))


  (setq y (- (cadr base-point) (/ *it-panel-height* 2)))

  ;; LEFT EDGE OF PANEL
  (setq panel-left (- (car base-point) (/ *it-panel-width* 2.0)))





  ;; DRAW EACH DEVICE ROW

  (setq row-y y)
  (setq is-first T)

  (setq row-index 0)

  (foreach row device-rows 

    (setq row-index (1+ row-index))
    (setq panel-bottom (- (cadr base-point) *it-panel-height*))

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

    (rb-set-layer *it-layer-cable*)
    (command "LINE" trunk-start trunk-end "")


    (if (> row-index 1) 
      (setq offset-x (+ (car trunk-end) 
                        (* row-index *it-riser-offset-step*)
                     )
      )
      (setq offset-x (car base-point))
    )

    ;; connect non-first rows upward
    (if (not is-first) 
      (progn 


        (rb-set-layer *it-layer-cable*)
        ;; horizontal segment
        (command "LINE" 
                 (list (car trunk-end) row-y)
                 (list offset-x row-y)
                 ""
        )

        ;; vertical segment
        (command "LINE" 
                 (list offset-x row-y)
                 (list offset-x panel-bottom)
                 ""
        )
      )
    )

    ;; calculate cable tag for this row

    (setq row-cables (get-it-row-cables 
                       (nth 0 panel)
                       row
                       cable-data
                     )
    )

    (setq wire-counts (count-it-cables row-cables))


    (setq wire-tag (format-it-cable-tag wire-counts))

    (it-draw-leader 

      (list 
        (- (car trunk-end) 1)
        (cadr trunk-end)
      )

      (list 
        (- (car trunk-end) 1)
        (+ (cadr trunk-end) 0.4)
      )

      wire-tag
    )

    ;; restart x position for every row
    (setq x (car trunk-start))

    (foreach device row 

      (setq dev-block (nth 2 device))



      (rb-set-layer *it-layer-cable*)
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
    (setq is-first nil)
  ) ;; end foreach row
) ;; end function


(defun it-insert-device (block-name insert-point) 
  (rb-set-layer *it-layer-device*)
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

  (rb-set-layer *it-layer-text*)
  (command "TEXT" pt *it-device-id-text-height* 0 label)
)


(defun it-draw-leader (wire-point text-point text) 
  (rb-set-layer *it-layer-cable*)
  (command 
    "_MLEADER"
    wire-point
    text-point
    (strcat text "")
    ""
  )
)



(defun it-set-attribute (entity tag value / att) 

  (setq att (entnext entity))

  (while att 

    (if (= "ATTRIB" (cdr (assoc 0 (entget att)))) 

      (if 
        (= (strcase tag) 
           (strcase (cdr (assoc 2 (entget att))))
        )

        (progn 

          (entmod 
            (subst 
              (cons 1 value)
              (assoc 1 (entget att))
              (entget att)
            )
          )

          (entupd att)
        )
      )
    )

    (setq att (entnext att))
  )
)



;; ============================================================
;; Draw Daisy Chain Loop
;;
;; Devices are drawn:
;;
;; D3 -------- D2 -------- D1 -------- PANEL
;;
;; First trunk uses:
;;   *it-daisy-first-trunk-length*
;;
;; Device spacing uses:
;;   *it-daisy-device-spacing*
;;
;; ============================================================


(defun it-layout-daisy-loop (panel loop-data base-point / loop-no devices panel-point 
                             device-point insert-point device block-name wire-tag 
                             cable first-device
                            ) 


  ;; extract loop information

  (setq loop-no (car loop-data))
  (setq devices (cdr loop-data))
  ;; panel connection point
  (setq panel-point base-point)
  ;; first device connection point

  (setq device-point (list 
                       (- (car panel-point) 
                          *it-daisy-first-trunk-length*
                       )

                       (cadr panel-point)
                     )
  )
  ;; Cable tag Use first device cable

  (setq first-device (car devices))
  (setq cable (nth 3 first-device))
  ;; draw cable leader on first trunk
  (it-draw-leader 
    (list 
      (/ 
        (+ (car panel-point) 
           (car device-point)
        )
        2.0
      )
      (cadr panel-point)
    )
    (list 

      (/ 
        (+ (car panel-point) 
           (car device-point)
        )
        2.0
      )

      (+ (cadr panel-point) 0.25)
    )
    cable
  )
  ;; Draw devices
  (foreach device devices 
    ;; Draw cable segment

    (command "LINE" 
             panel-point
             device-point
             ""
    )

    ;; Convert connection point to block insertion point
    (setq insert-point (list 

                         (- (car device-point) 
                            (/ *it-device-width* 2.0)
                         )

                         (+ (cadr device-point) 
                            (/ *it-device-height* 2.0)
                         )
                       )
    )
    ;; block name
    (setq block-name (nth 2 device))
    ;; insert device

    (it-insert-device 
      block-name
      insert-point
    )
    ;; device label

    (it-place-device-id 
      device
      insert-point
    )

    ;; Move pattern left
    (setq panel-point (list 

                        (- (car device-point) 
                           *it-device-width*
                        )

                        (cadr device-point)
                      )
    )

    (setq device-point (list 

                         (- (car panel-point) 
                            *it-daisy-device-spacing*
                         )

                         (cadr panel-point)
                       )
    )
  )
)

;; Draw All Daisy Chain Loops For Panel
;; D3 -------- D2 -------- D1 -------- PANEL

(defun it-layout-daisy-loops (panel base-point / devices loops loop row-y) 

  ;; get all daisy devices
  (setq devices (get-it-daisy-devices panel))
  ;; group by loop number
  (setq loops (get-it-daisy-loops devices))
  ;; start first loop at panel connection point
  (setq row-y (cadr base-point))
  ;; draw every loop
  (foreach loop loops 
    (it-layout-daisy-loop 
      panel
      loop
      (list 
        (car base-point)
        row-y
      )
    )

    ;; move down for next loop
    (setq row-y (- row-y *it-row-spacing*))
  )
)


  ;; ------------------------------------------------------------
  ;; Draw Complete Intrusion Riser
  ;; ------------------------------------------------------------
(defun IT-DRAW-RISER (system-data cable-data / y old-osnap panel-height) 

  (prompt "\n--- Drawing Intrusion Riser ---")



  ;; Disable osnap
  (setq old-osnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; Starting Y position
  (setq y 0)


  (setq y 0)


  (foreach panel system-data 

    ;; calculate current panel height
    (setq panel-height (it-get-panel-layout-height panel))





    ;; draw panel
    (it-layout-panel 
      panel
      (list 0 y)
      cable-data
    )


    ;; move upward for next panel
    (setq y (+ y 
               panel-height
               *it-panel-spacing*
            )
    )
  )

  ;; Restore osnap
  (setvar "OSMODE" old-osnap)

  (prompt "\nIntrusion riser complete.")
  (princ)
)