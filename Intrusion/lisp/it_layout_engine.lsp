;; ============================================================
;; Intrusion Riser Layout Engine
;;
;; Purpose:
;;   Generates complete intrusion system riser drawings from
;;   structured panel/device data.
;;
;; Main responsibilities:
;;   - Insert panel and PSU blocks
;;   - Layout home-run devices in rows
;;   - Layout daisy-chain loops by loop number
;;   - Layout keypad daisy chains separately
;;   - Draw cable paths and cable labels
;;   - Place device blocks and IDs
;;
;; Input:
;;   Panel data model:
;;
;;   (
;;     Panel ID
;;     Panel Type
;;     Panel Block
;;     Devices
;;   )
;;
;; Device structure:
;;
;;   (
;;     Device ID
;;     Device Type
;;     Block Name
;;     Cable
;;     Loop Type
;;     Loop Number
;;   )
;;
;; Drawing rules:
;;   - Home runs are drawn left of panel
;;   - Daisy chains are drawn below home runs
;;   - Keypads are drawn right of panel
;;   - External PSU is placed above panel
;;   - Each panel is processed independently
;;
;; Entry Point:
;;   IT-DRAW-RISER
;;
;; ============================================================




;; Controls complete layout process for one panel
;; Draws panel, PSU, home runs, daisy loops, and keypads
(defun it-layout-panel (panel base-point cable-data / panel-type block-name psu-block 
                        psu-point panel-entity home-run-devices home-run-rows daisy-y 
                        keypad-y keypad-devices
                       ) 
  (setq row-index 0)

  ;; Get panel type and block definition
  (setq panel-type (nth 1 panel))

  (setq block-name (get-it-panel-block panel-type))

  ;; Insert main panel block
  (setq panel-entity (it-insert-panel block-name base-point))

  ;; Insert external PSU above panel when required
  (if (it-panel-requires-psu panel-type) 

    (progn 
      (setq psu-block (get-it-panel-ps-block panel-type))

      ;; Calculate PSU insertion point
      (setq psu-point (list 
                        (car base-point)
                        (+ (cadr base-point) 
                           *it-panel-height*
                           *it-psu-vertical-spacing*
                        )
                      )
      )

      (it-insert-psu psu-block psu-point)
    )
  )
  ;; Update panel ID attribute
  (it-set-attribute 
    panel-entity
    "PANEL_ID"
    (nth 0 panel)
  )


  ;; Draw home-run devices and track occupied rows
  (setq row-index (it-layout-home-runs 
                    panel
                    base-point
                    cable-data
                    row-index
                  )
  )

  ;; Calculate keypad starting position on right side
  (setq keypad-devices (get-it-keypad-devices panel))

  (setq keypad-y (- 
                   (cadr base-point)
                   (/ *it-panel-height* 2.0)
                 )
  )

  ;; Calculate daisy-chain starting position
  (setq home-run-devices (get-it-home-run-devices panel))

  (setq home-run-rows (length 
                        (split-it-device-rows home-run-devices)
                      )
  )

  (setq daisy-y (- 
                  (cadr base-point)
                  (/ *it-panel-height* 2.0)
                  (* row-index *it-row-spacing*)
                )
  )


  ;;; Draw standard daisy-chain loops on left side
  (it-layout-daisy-loops 
    panel
    (list 
      (- 
        (car base-point)
        (/ *it-panel-width* 2.0)
      )
      daisy-y
    )
    row-index
  )

  ;; Draw keypad chain on right side (only if present)
  (if keypad-devices 
    (it-layout-keypads 
      panel
      (list 
        (+ 
          (car base-point)
          (/ *it-panel-width* 2.0)
        )
        keypad-y
      )
    )
  )
)


;; Draws all home-run loops from panel to all devices
;; Devices are placed from panel outward in sequence
;; ------------------------ PANEL
;; |     |     |     |
;; D1    D2    D3    D4

(defun it-layout-home-runs (panel base-point cable-data row-index / devices 
                            device-rows panel-left trunk-start trunk-end x y row-y 
                            dev-block cable wire-point text-point row-cables 
                            wire-counts wire-tag row
                           ) 

  ;; Get home-run devices and split them into drawing rows
  (setq devices (get-it-home-run-devices panel))
  (setq device-rows (split-it-device-rows devices))
  (setq rows device-rows) ;;

  ;; Calculate starting position below panel
  (setq y (- (cadr base-point) (/ *it-panel-height* 2)))

  ;; Calculate left edge of panel for cable trunk connection
  (setq panel-left (- (car base-point) (/ *it-panel-width* 2.0)))

  ;; Draw each home-run row
  (setq row-y y)
  (setq is-first T)

  (foreach row device-rows 

    ;; Track number of occupied rows
    (setq row-index (1+ row-index))

    ;; Create horizontal trunk from devices to panel
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

    ;; Create vertical riser connection for additional rows
    (if (> row-index 1) 
      (setq offset-x (+ (car trunk-end) 
                        (* row-index *it-riser-offset-step*)
                     )
      )
      (setq offset-x (car base-point))
    )

    (if (not is-first) 
      (progn 

        (rb-set-layer *it-layer-cable*)
        ;;; Horizontal connection to riser
        (command "LINE" 
                 (list (car trunk-end) row-y)
                 (list offset-x row-y)
                 ""
        )
        ;; Vertical connection back to panel
        (command "LINE" 
                 (list offset-x row-y)
                 (list offset-x panel-bottom)
                 ""
        )
      )
    )
    ;; Generate cable tag for row trunk
    (setq row-cables (get-it-row-cables 
                       (nth 0 panel)
                       row
                       cable-data
                     )
    )
    (setq wire-counts (count-it-cables row-cables))
    (setq wire-tag (format-it-cable-tag wire-counts))

    ;; Add cable leader for row trunk
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

    ;; Reset device position for current row
    (setq x (car trunk-start))

    ;; Draw devices and drops
    (foreach device row 
      (setq dev-block (nth 2 device))

      (rb-set-layer *it-layer-cable*)
      ;; Draw vertical device drop
      (command "LINE" 
               (list x row-y)
               (list x (- row-y *it-device-drop*))
               ""
      )

      ;; Insert device block
      (it-insert-device 
        dev-block
        (list x (- row-y *it-device-drop*))
      )

      ;; Add device identifier
      (it-place-device-id 
        device
        (list x (- row-y *it-device-drop*))
      )

      ;; Get device cable information
      (setq cable (get-it-device-cable 
                    (nth 0 panel)
                    (nth 0 device)
                    cable-data
                  )
      )

      ;; Draw device cable leader
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

      ;; Move right for next device
      (setq x (+ x *it-device-spacing*))
    ) ;; end foreach device


    ;; Move down for next device row
    (setq row-y (- row-y *it-row-spacing*))


    (setq is-first nil)
  ) ;; end foreach row

  ;; Return total number of occupied rows
  (setq row-index (length device-rows))
  row-index
) ;; end function



;; Draws one daisy-chain loop from panel to all loop devices
;; Devices are placed from panel outward in sequence
;; D3 -------- D2 -------- D1 -------- PANEL

(defun it-layout-daisy-loop (panel loop-data base-point / loop-no devices panel-point 
                             device-point insert-point device block-name wire-tag 
                             cable first-device
                            ) 


  ;; Extract loop number and devices
  (setq loop-no (car loop-data))
  (setq devices (cdr loop-data))

  ;; Starting connection point at panel
  (setq panel-point base-point)

  ;; Calculate first device connection point
  (setq device-point (list 
                       (- (car panel-point) 
                          *it-daisy-first-trunk-length*
                       )

                       (cadr panel-point)
                     )
  )
  ;; Use first device cable type for trunk leader
  (setq first-device (car devices))
  (setq cable (nth 3 first-device))

  ;; Draw cable leader on main trunk
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

  ;; Insert devices along daisy chain path
  (foreach device devices 

    ;; Draw cable segment between points
    (rb-set-layer *it-layer-cable*)
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
    ;; Insert device block
    (setq block-name (nth 2 device))

    (it-insert-device 
      block-name
      insert-point
    )

    ;; Add device identifier
    (it-place-device-id 
      device
      insert-point
    )

    ;; Move to next device position
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

;; Draws all daisy-chain loops for a panel
;; Each loop is placed on its own horizontal row
;; D3 -------- D2 -------- D1 -------- PANEL
;; D6 -------- D5 -------- D4 --------


(defun it-layout-daisy-loops (panel base-point row-index / devices loops loop row-y 
                              row-index offset-x panel-bottom
                             ) 

  ;; Get daisy-chain devices and group them by loop number
  (setq devices (get-it-daisy-devices panel))
  (setq loops (get-it-daisy-loops devices))

  ;; Start first loop at panel connection height
  (setq row-y (cadr base-point))

  ;; Calculate bottom of panel for vertical riser connections
  (setq panel-bottom (- (cadr base-point) 
                        (/ *it-panel-height* 2.0)
                     )
  )

  ;; Draw each daisy-chain loop
  (foreach loop loops 

    ;; Add offset riser when loops are below previous layouts
    (if (> row-index 0) 

      (progn 

        ;; Calculate staggered riser position
        (setq offset-x (+ (car base-point) 
                          (* row-index *it-riser-offset-step*)
                       )
        )


        ;; Horizontal connection to riser
        (rb-set-layer *it-layer-cable*)
        (command "LINE" 
                 (list (car base-point) row-y)
                 (list offset-x row-y)
                 ""
        )


        ;; Vertical connection back to panel
        (command "LINE" 
                 (list offset-x row-y)
                 (list offset-x panel-bottom)
                 ""
        )
      )
    )

    ;; Draw individual loop devices
    (it-layout-daisy-loop 
      panel
      loop
      (list 
        (car base-point)
        row-y
      )
    )

    ;; Move down for next loop
    (setq row-y (- row-y *it-row-spacing*))
    ;; Increase row counter for next loop offset
    (setq row-index (1+ row-index))
  )
)


;; Draws keypad daisy chain on right side of panel
;; Keypads are handled separately from standard daisy loops
;; PANEL --------KP -------- KP -------- KP

(defun it-layout-keypads (panel base-point / keypads panel-point device-point device 
                          block-name insert-point
                         ) 
  ;; Get keypad devices
  (setq keypads (get-it-keypad-devices panel))


  ;; Starting point at panel connection
  (setq panel-point base-point)


  ;; Calculate first keypad connection point
  (setq device-point (list 
                       (+ (car panel-point) *it-daisy-first-trunk-length*)
                       (cadr panel-point)
                     )
  )

  ;; Draw cable leader for keypad trunk
  (it-draw-leader 

    ;; Arrow location at cable midpoint
    (list 
      (/ 
        (+ (car panel-point) 
           (car device-point)
        )
        2.0
      )
      (cadr panel-point)
    )

    ;; Leader text location
    (list 
      (/ 
        (+ (car panel-point) 
           (car device-point)
        )
        2.0
      )
      (+ (cadr panel-point) 0.25)
    )

    ;; cable type from first keypad
    (nth 3 (car keypads))
  )

  ;; Draw each keypad
  (foreach device keypads 

    ;; Draw cable segment
    (rb-set-layer *it-layer-cable*)

    (command "LINE" 
             panel-point
             device-point
             ""
    )

    ;; Get keypad block name
    (setq block-name (nth 2 device))


    ;; Convert connection point to insertion point
    (setq insert-point (list 
                         (- (car device-point) 
                            (/ *it-device-width* 2.0)
                         )

                         (+ (cadr device-point) 
                            (/ *it-device-height* 2.0)
                         )
                       )
    )


    ;; Insert keypad block
    (it-insert-device 
      block-name
      insert-point
    )


    ;; Add keypad identifier
    (it-place-device-id 
      device
      insert-point
    )


    ;; Move to next keypad location
    (setq panel-point device-point)


    (setq device-point (list 
                         (+ (car device-point) 
                            *it-daisy-device-spacing*
                         )
                         (cadr device-point)
                       )
    )
  )
)



;;Helpers
;; Inserts panel block and returns inserted entity reference
(defun it-insert-panel (block-name insert-point / ent) 
  (rb-set-layer *it-layer-device*)
  (command "_-INSERT" block-name insert-point 1 1 0 "")

  (setq ent (entlast))

  ent
)

;; Inserts external PSU block on device layer
(defun it-insert-psu (block-name insert-point) 

  (rb-set-layer *it-layer-device*)

  (command "_-INSERT" block-name insert-point 1 1 0 "")
)


 ;; Creates layer if missing and sets it as current layer
(defun rb-set-layer (layer-name) 
  (if (not (tblsearch "LAYER" layer-name)) 
    (command "-LAYER" "M" layer-name "")
  )
  (setvar "CLAYER" layer-name)
)

;; Returns connection point shifted right by device width
(defun get-it-daisy-right-point (insert-point) 

  (list 
    (+ (car insert-point) 
       *it-device-width*
    )

    (cadr insert-point)
  )
)

;; Inserts device block on device layer
(defun it-insert-device (block-name insert-point) 
  (rb-set-layer *it-layer-device*)
  (command "_-INSERT" block-name insert-point 1 1 0 "")
)

;; Places device identifier text near device block
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

;; Creates cable leader with supplied cable label
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

;; Updates matching attribute value in block reference (for panel ID)
(defun it-set-attribute (entity tag value / att) 

  (setq att (entnext entity))

  ;; Search through block attributes
  (while att 

    (if (= "ATTRIB" (cdr (assoc 0 (entget att)))) 

      ;; Check attribute tag and update value
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


(defun IT-DRAW-RISER (system-data cable-data / y old-osnap panel-height idx) 

  (prompt "\n--- Drawing Intrusion Riser ---")

  ;; Disable osnap
  (setq old-osnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; Start Y
  (setq y 0)
  (setq idx 0)

  ; (it-debug-log "============================")
  ; (it-debug-log "START RISER DRAW")

  ;; Loop panels
  (foreach panel system-data 

    (setq idx (1+ idx))

    ; ;; Log BEFORE calculation
    ; (it-debug-log (strcat "\nPanel #" (itoa idx)))
    ; (it-debug-log (strcat "Start Y: " (rtos y 2 3)))

    ;; Calculate height
    (setq panel-height (it-get-panel-layout-height panel))

    ; (it-debug-log (strcat "Panel height: " (rtos panel-height 2 3)))
    ; (it-debug-log (strcat "Panel spacing: " (rtos *it-panel-spacing* 2 3)))

    ;; Draw panel
    (it-layout-panel 
      panel
      (list 0 y)
      cable-data
    )

    ;; Calculate next Y
    (setq next-panel (cadr (member panel system-data)))

    (if next-panel 
      (setq next-height (it-get-panel-layout-height next-panel))
      (setq next-height 0)
    )

    (setq y (+ y next-height *it-panel-spacing*))

    ;; Log AFTER movement
    ; (it-debug-log (strcat "Next Y: " (rtos y 2 3)))
  )

  ;; Restore osnap
  (setvar "OSMODE" old-osnap)

  ; (it-debug-log "END RISER DRAW")
  ; (it-debug-log "============================")

  (prompt "\nIntrusion riser complete.")
  (princ)
)

