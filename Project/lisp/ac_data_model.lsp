;; ============================================================
;; Access Control Data Model Builder
;;
;; Converts parsed CSV data into structured panel, door,
;; device, and cable models used by the layout engine.
;;
;; ============================================================



;; Splits device string from CSV into device list.
;;
;; Example:
;; "CR,DC,ES" -> ("CR" "DC" "ES")

(defun split-devices (str) 
  (if str 
    (parseCSV str)
    nil
  )
)

;; Splits cable string from CSV into cable list.
(defun split-cables (str) 
  (if str 
    (parseCSV str)
    nil
  )
)

;; Finds an existing panel in the panel model.
;;
;; Panel structure:
;; ("ACP-01" "ACP" "ACP_FULL" doors)
;;
;; Panel ID is stored as the first item.

(defun find-panel (panel-id panels) 
  (assoc panel-id panels)
)



;; Creates a new panel data structure.
;;
;; Stores:
;;   Panel ID
;;   Panel Type
;;   Panel Block
;;   Door list


(defun create-panel (panel-id panel-type panel-block door-id devices) 

  (list 
    panel-id
    panel-type
    panel-block
    (list 
      (list door-id devices)
    )
  )
)



;; Adds a new door and devices to an existing panel.
(defun add-door-to-panel (panel door-id devices / doors) 

  ;; get existing doors
  (setq doors (nth 3 panel))

  ;; rebuild panel
  (list 
    (nth 0 panel) ;; panel ID
    (nth 1 panel) ;; panel type
    (nth 2 panel) ;; panel block
    (append 
      doors
      (list (list door-id devices))
    )
  )
)

;; Replaces an existing panel entry in the panel list.
(defun replace-panel (old-panel new-panel panels / result) 

  (setq result '())

  (foreach panel panels 
    (if (= (car panel) (car old-panel)) 
      (setq result (cons new-panel result))
      (setq result (cons panel result))
    )
  )

  (reverse result)
)


;; Builds the access control panel data model.
;;
;; Input:
;;   raw-data - Parsed CSV data
;;
;; Returns:
;;   List of panels with associated doors and devices

(defun build-data-model (raw-data / panels row panel-id panel-type panel-block 
                         door-id devices existing-panel
                        ) 
  (setq panels '())
  (foreach row raw-data 
    ;; CSV columns
    (setq panel-id (nth 2 row))
    (setq panel-type (nth 3 row))
    (setq panel-block (nth 4 row))
    (setq door-id (nth 1 row))
    (setq devices (split-devices 
                    (nth 7 row)
                  )
    )

    ;; search panel
    (setq existing-panel (find-panel panel-id panels))
    (if existing-panel 
      ;; EXISTING PANEL
      (setq panels (replace-panel 
                     existing-panel
                     (add-door-to-panel 
                       existing-panel
                       door-id
                       devices
                     )
                     panels
                   )
      )
      ;; NEW PANEL
      (setq panels (cons 
                     (create-panel panel-id panel-type panel-block door-id devices)
                     panels
                   )
      )
    )
  )

  ;; keep original order
  (reverse panels)
)


;; Builds cable data model from CSV data.
;;
;; Returns:
;;   List containing panel, door, and cable information.
(defun build-cable-model (raw-data / cables row panel-id door-id cable-list) 

  (setq cables '())

  (foreach row raw-data 

    (setq panel-id (nth 2 row))

    (setq door-id (nth 1 row))

    (setq cable-list (split-cables 
                       (nth 5 row)
                     )
    )

    (setq cables (cons 
                   (list 
                     panel-id
                     door-id
                     cable-list
                   )
                   cables
                 )
    )
  )

  (reverse cables)
)

;; Gets all cable entries belonging to a panel.
(defun get-panel-cables (panel-id cable-data / result item) 

  (setq result '())

  (foreach item cable-data 
    (if (= panel-id (nth 0 item)) 
      (setq result (cons item result))
    )
  )

  (reverse result)
)

;; Gets cables for a specific row of doors.
(defun get-row-cables (panel-cables start-index count / result index) 

  (setq result '())
  (setq index 0)

  (foreach cable panel-cables 

    (if 
      (and 
        (>= index start-index)
        (< index (+ start-index count))
      )

      (setq result (cons 
                     (nth 2 cable)
                     result
                   )
      )
    )

    (setq index (+ index 1))
  )

  (reverse result)
)

;; Counts cable quantities for a group of cable lists.
(defun count-cables (cable-lists / counts cable) 

  (setq counts '())

  (foreach cable-list cable-lists 

    (foreach cable cable-list 

      (if (assoc cable counts) 

        (setq counts (subst 
                       (cons cable (+ 1 (cdr (assoc cable counts))))
                       (assoc cable counts)
                       counts
                     )
        )

        (setq counts (cons 
                       (cons cable 1)
                       counts
                     )
        )
      )
    )
  )

  counts
)

;; Formats cable counts into riser cable tag format.
;;
;; Example:
;; A x2, R x1 -> 2A,1R
(defun format-cable-tag (counts / result sorted) 

  ;; sort alphabetically by cable name
  (setq sorted (vl-sort 
                 counts
                 '(lambda (a b) 
                    (< (car a) (car b))
                  )
               )
  )

  (setq result "")

  (foreach item sorted 

    (setq result (strcat 
                   result
                   (if (= result "") 
                     ""
                     ","
                   )
                   (itoa (cdr item))
                   (car item)
                 )
    )
  )

  result
)

;; Gets cable information for a specific door.
(defun get-door-cables (panel-id door-id cable-data / result item) 

  (setq result nil)

  (foreach item cable-data 

    (if 
      (and 
        (= panel-id (nth 0 item))
        (= door-id (nth 1 item))
      )
      (setq result (nth 2 item))
    )
  )

  result
)

