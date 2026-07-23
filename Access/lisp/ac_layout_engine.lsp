;; ============================================================
;; Access Control Riser Layout Engine
;;
;; Responsible for generating the riser drawing:
;; - Panel placement
;; - PSU placement
;; - Horizontal wiring
;; - Door drops
;; - Device placement
;; - Cable tags
;; - Door labels
;;
;; ============================================================

;; Calculates the panel wire connection point.
;;
;; The panel block insertion point is top-center.
;; Connection point is calculated at the bottom center.
(defun rb-get-panel-connection-point (base-point / x y) 

  ;; block base point is TOP CENTER
  (setq x (car base-point))
  (setq y (cadr base-point))
  (list 
    x
    (- y (/ *rb-panel-height* 2.0))
  )
)



;; Main panel layout function.
;;
;; Creates the complete riser layout for one panel:
;; - Inserts panel block
;; - Inserts PSU block if required
;; - Creates horizontal wire rows
;; - Creates door branches
;; - Inserts devices
;; - Adds cable tags and door IDs
;;
;; Input:
;;   panel      - Panel data model
;;   base-point - Starting drawing location
;;   cable-data - Cable information model
(defun rb-layout-panel (panel base-point cable-data / panel-id panel-type panel-block 
                        doors panel-connection-point door-count horizontal-line-end 
                        door-index door-position door-drop-point device-index col row 
                        pt blk door-row door-column row-start-point row-line-end 
                        row-door-count door-id-point panel-cables row-cables 
                        wire-counts wire-tag wire-point text-point door-cables 
                        door-wire-tag door-wire-point door-text-point horizontal-ent 
                        vertical-ent vertical-start vertical-end panel-insert-point 
                        device-count
                       ) 



  ;; Get panel information from data model
  (setq panel-id (nth 0 panel))

  (setq panel-type (nth 1 panel))

  ;; Get panel block from library
  (setq panel-block (get-panel-block panel-type))

  ;; Calculate panel insertion location
  (setq panel-insert-point (list 
                             (+ (car base-point) *rb-panel-half-width*)
                             (cadr base-point)
                           )
  )

  ;; Insert panel block
  (rb-insert-panel 
    panel-block
    panel-insert-point
  )

  ;; Insert external PSU block when required
  (if (panel-requires-psu panel-type) 

    (rb-insert-panel 

      (get-panel-ps-block panel-type)

      (list 
        (+ 
          (car panel-insert-point)
          *rb-panel-width*
          *rb-ps-offset-x*
        )
        (cadr panel-insert-point)
      )
    )
  )

  ;; Get panel doors and cable information
  (setq doors (nth 3 panel))

  (setq panel-cables (get-panel-cables 
                       panel-id
                       cable-data
                     )
  )


  ;; Calculate wire connection point below panel
  (setq panel-connection-point (rb-get-panel-connection-point base-point))

  (setq door-count (length doors))

  ;; Process each door connected to panel
  (setq door-index 0)
  (foreach door doors 

    ;; Determine door row and column position
    (setq door-column (rem door-index *rb-door-limit*))

    (setq door-row (fix (/ door-index *rb-door-limit*)))

    ;; Calculate number of doors on current row
    (setq row-door-count (min 
                           *rb-door-limit*
                           (- door-count (* door-row *rb-door-limit*))
                         )
    )

    ;; Calculate starting point for current row
    (setq row-start-point (list 
                            (car panel-connection-point)
                            (- 
                              (cadr panel-connection-point)
                              (* door-row *rb-door-row-gap*)
                            )
                          )
    )

    ;; Calculate end point of horizontal trunk line
    (setq row-line-end (rb-get-horizontal-line-end 
                         row-start-point
                         row-door-count
                       )
    )


    ;; Add cable tag for each horizontal riser row.
    ;;
    ;; Only runs once per row (first door position).
    ;; Calculates total cable quantities and creates tag.
    (if (= door-column 0) 
      (progn 
        (setq row-cables (get-row-cables 
                           panel-cables
                           (* door-row *rb-door-limit*)
                           row-door-count
                         )
        )

        (setq wire-counts (count-cables row-cables))

        (setq wire-tag (format-cable-tag wire-counts))


        (setq wire-point (list 
                           (+ (car row-start-point) *rb-wire-tag-offset*)
                           (cadr row-start-point)
                         )
        )

        (setq text-point (list 
                           (+ (car wire-point))
                           (+ (cadr wire-point) 0.25)
                         )
        )

        (rb-draw-leader 
          wire-point
          text-point
          wire-tag
        )
      )
    )


    ;; Draw horizontal trunk line for each door row.
    ;;
    ;; Additional rows are connected back to panel using
    ;; vertical line and chamfer transition.

    (if (= door-column 0) 

      (progn 

        ;; draw horizontal row line
        (setq horizontal-ent (rb-draw-line 
                               row-start-point
                               row-line-end
                             )
        )


        ;; only chamfer additional rows
        (if (> door-row 0) 

          (progn 

            ;; vertical line starts from bottom of panel block
            (setq vertical-start (list 
                                   (- (car panel-insert-point) 0.3)
                                   (- 
                                     (cadr panel-insert-point)
                                     *rb-panel-height*
                                   )
                                 )
            )


            ;; vertical goes downward
            (setq vertical-end (list 
                                 (car vertical-start)
                                 (- (cadr vertical-start) 1.0)
                               )
            )


            ;; draw vertical
            (setq vertical-ent (rb-draw-line 
                                 vertical-start
                                 vertical-end
                               )
            )


            ;; chamfer
            (command "_CHAMFER" "_D" 0 0 "")


            (command 
              "_CHAMFER"
              horizontal-ent
              vertical-ent
            )
          )
        )
      )
    )

    ;; Calculate door branch location
    (setq door-position (rb-get-door-position 
                          row-line-end
                          door-column
                          (cadr row-start-point)
                        )
    )

    ;; Calculate bottom of door drop
    (setq door-drop-point (rb-get-door-drop-point 
                            door-position
                          )
    )

    ;; Draw vertical door branch
    (rb-draw-line 
      door-position
      door-drop-point
    )

    ;; Create door cable tag
    ;;
    ;; Uses cable information assigned to this door.
    (setq door-cables (get-door-cables 
                        panel-id
                        (car door)
                        cable-data
                      )
    )
    (setq door-wire-tag (apply 'strcat 
                               (mapcar 
                                 '(lambda (x) 
                                    (strcat x ",")
                                  )
                                 door-cables
                               )
                        )
    )

    (setq door-wire-tag (substr 
                          door-wire-tag
                          1
                          (- (strlen door-wire-tag) 1)
                        )
    )

    ;; Place door cable leader halfway down branch
    (setq door-wire-point (list 
                            (car door-position)
                            (/ 
                              (+ (cadr door-position) 
                                 (cadr door-drop-point)
                              )
                              2.0
                            )
                          )
    )


    (setq door-text-point (list 
                            (+ (car door-wire-point) 0.25)
                            (cadr door-wire-point)
                          )
    )


    (rb-draw-leader 
      door-wire-point
      door-text-point
      door-wire-tag
    )


    ;; Insert devices connected to door.
    ;;
    ;; Devices are arranged:
    ;; - two columns
    ;; - multiple rows

    (setq device-index 0)
    (setq device-count (length (cadr door)))
    (foreach device (cadr door) 
      (setq col (rem device-index 2))
      (setq row (fix (/ device-index 2)))

      (setq pt (rb-get-device-position 
                 door-drop-point
                 col
                 row
                 device-count
               )
      )

      (setq blk (get-device-block device))


      (if blk 
        (rb-insert-device blk pt)
        (print (strcat "Missing device: " device))
      )


      (setq device-index (+ device-index 1))
    )

     ;; Insert door identification label
    (setq door-id-point (list 
                          (+ (car door-drop-point) *rb-door-id-x-offset*)
                          (+ 
                            (cadr door-drop-point)
                            *rb-door-id-offset*
                          )
                        )
    )

    (rb-insert-door-id 
      (car door)
      door-id-point
    )

    (setq door-index (+ door-index 1))
  )
)



;; Calculates end point of horizontal trunk line.
;;
;; Extends line left based on number of doors.
(defun rb-get-horizontal-line-end (connection-point door-count / x y line-length) 

  (setq x (car connection-point))
  (setq y (cadr connection-point))


  ;; each door gets spacing

  (setq line-length (* door-count *rb-door-spacing*))


  ;; extend left

  (list 
    (- x line-length)
    y
  )
)

;; Calculates door branch location on trunk line.
(defun rb-get-door-position (line-start index line-y / x) 

  ;; calculate x position for this door
  (setq x (+ 
            (car line-start)
            (* index *rb-door-spacing*)
          )
  )
  ;; return branch point
  (list x line-y)
)


;; Calculates bottom point of door vertical drop.
(defun rb-get-door-drop-point (branch-point / x y) 

  (setq x (car branch-point))
  (setq y (cadr branch-point))
  (list 
    x
    (- y *rb-door-drop*)
  )
)

;; Draws wire line on security wire layer.
(defun rb-draw-line (pt1 pt2) 

  (rb-set-layer "E-SEC-WIRE")

  (command 
    "_LINE"
    pt1
    pt2
    ""
  )

  (entlast)
)


;; Calculates device insertion point.
;;
;; Single device:
;;   centered on door wire
;;
;; Multiple devices:
;;   arranged in two columns
(defun rb-get-device-position (door-drop-point col row device-count / x y) 

  (setq x (car door-drop-point))
  (setq y (cadr door-drop-point))


  ;; horizontal placement
  (cond 

    ;; Single device - centered on vertical wire
    ((= device-count 1)

     (setq x x)
    )


    ;; Multiple devices - split left/right
    (T

     (setq x (+ x 
                (if (= col 0) 
                  (- (/ *rb-device-column-gap* 2))
                  (/ *rb-device-column-gap* 2)
                )
             )
     )
    )
  )


  ;; vertical rows
  (setq y (- y 
             (* row *rb-device-row-gap*)
          )
  )


  (list x y)
)



;; Inserts device block into drawing.
(defun rb-insert-device (block-name insert-point) 


  (rb-set-layer "E-SERT-ACCS")

  (command "_-INSERT" block-name insert-point 1 1 0 "")
)

;; Sets current AutoCAD layer.
;;
;; Creates layer if it does not exist.
(defun rb-set-layer (layer-name) 

  (if (tblsearch "LAYER" layer-name) 

    (setvar "CLAYER" layer-name)

    (command 
      "-LAYER"
      "M"
      layer-name
      ""
    )
  )
)

;; Inserts door ID text label.
;;
;; Text color is set to white (ACI 7).
(defun rb-insert-door-id (door-id point / ent) 

  (rb-set-layer "E-SERT-ACCS")

  (setvar "TEXTSTYLE" *rb-door-id-text-style*)

  (command "_TEXT" point *rb-door-id-text-height* 0 door-id)

  ;; get created text entity
  (setq ent (entlast))

  ;; set color to white (ACI 7)
  (entmod 
    (append 
      (entget ent)
      '((62 . 7))
    )
  )
)

;; Creates cable multileader annotation.
(defun rb-draw-leader (wire-point text-point text / oldstyle) 

  (rb-set-layer "E-SEC-WIRE")

  ;; Save current style
  ; (setq oldstyle (getvar "CMLEADERSTYLE"))

  ;; Set your multileader style
  (setvar "CMLEADERSTYLE" "AnnoWireTag")

  ;; Draw multileader
  (command 
    "_MLEADER"
    wire-point
    text-point
    text
  )

  ;; Restore previous style
  ; (setvar "CMLEADERSTYLE" oldstyle)
)

;; Inserts panel block.
(defun rb-insert-panel (block-name insert-point) 

  (rb-set-layer "E-SERT-ACCS")

  (command "_-INSERT" block-name insert-point 1 1 0 "")
)


;; Main riser generation command.
;;
;; Uses loaded input data to:
;; - build panel model
;; - build cable model
;; - generate complete riser drawing
(defun DRAW_RISER (/ system-data cable-data old-osnap y) 

  ;; make sure data is loaded
  (if (not *rb-input-data*) 
    (progn 
      (prompt "\nERROR: Input data not loaded. Run RB-INIT first.")
      (exit)
    )
  )

  ;; build models
  (setq system-data (build-data-model *rb-input-data*))

  (setq cable-data (build-cable-model *rb-input-data*))

  ;; disable osnap
  (setq old-osnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; layout panels
  (setq y 0)

  (foreach panel system-data 

    (rb-layout-panel 
      panel
      (list 0 y)
      cable-data
    )

    (setq y (+ y *rb-panel-spacing*))
  )

  ;; restore osnap
  (setvar "OSMODE" old-osnap)

  (princ)
)