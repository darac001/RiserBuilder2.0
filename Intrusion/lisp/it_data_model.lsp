;; ============================================================
;; Intrusion Riser Data Model Builder
;;
;; Converts parsed CSV data into structured panel/device model.
;;
;; ============================================================



;; Finds an existing intrusion panel.
;;
;; Panel structure:
;;
;; (
;;   Panel ID
;;   Panel Type
;;   Panel Block
;;   Devices
;; )
;;
;; Example:
;;
;; (
;;  "ACP-01"
;;  "IDCP"
;;  "IDCP_FULL"
;;  (
;;    device1
;;    device2
;;  )
;; )
;;

(defun find-it-panel (panel-id panels) 

  (assoc panel-id panels)
)



;; Creates a new panel structure.

(defun create-it-panel (panel-id panel-type panel-block device) 

  (list 

    panel-id

    panel-type

    panel-block

    (list device)
  )
)



;; Adds a device to an existing panel.

(defun add-it-device-to-panel (panel device / devices) 

  ;; Existing devices
  (setq devices (nth 3 panel))


  ;; Return rebuilt panel

  (list 

    (nth 0 panel)
    (nth 1 panel)
    (nth 2 panel)

    (append 
      devices
      (list device)
    )
  )
)



;; Replace updated panel in model.

(defun replace-it-panel (old-panel new-panel panels / result) 


  (setq result '())


  (foreach panel panels 

    (if (= (car panel) (car old-panel)) 

      (setq result (cons new-panel result))

      (setq result (cons panel result))
    )
  )


  (reverse result)
)



;; Creates one intrusion device entry.
;;
;; CSV:
;;
;; Device ID
;; Devices
;; Device Block Name
;; Cable
;; Loop Type
;;
;; Model:
;;
;; (
;; Device ID
;; Acronym
;; Block
;; Cable
;; Loop Type
;; )
;;

(defun create-it-device (row) 

  (list 

    (nth 1 row) ;; Device ID

    (nth 6 row) ;; Device acronym

    (nth 7 row) ;; Block name

    (nth 5 row) ;; Cable

    (nth 8 row) ;; Loop type

    (nth 9 row) ;; Loop number
  )
)



;; Main intrusion data model builder.

(defun build-it-data-model (raw-data / panels row panel-id panel-type panel-block 
                            device existing-panel
                           ) 


  (setq panels '())


  (foreach row raw-data 


    ;; CSV columns

    (setq panel-id (nth 2 row))


    (setq panel-type (nth 3 row))


    (setq panel-block (nth 4 row))


    ;; create device object

    (setq device (create-it-device row))


    ;; check if panel exists

    (setq existing-panel (find-it-panel panel-id panels))


    (if existing-panel 


      ;; Existing panel

      (setq panels (replace-it-panel 

                     existing-panel

                     (add-it-device-to-panel 
                       existing-panel
                       device
                     )

                     panels
                   )
      )


      ;; New panel

      (setq panels (cons 

                     (create-it-panel 
                       panel-id
                       panel-type
                       panel-block
                       device
                     )

                     panels
                   )
      )
    )
  )


  ;; preserve CSV order

  (reverse panels)
)



;; ============================================================
;; Intrusion Cable Data Model
;;
;; Builds cable information from input CSV
;;
;; ============================================================



;; ------------------------------------------------------------
;; Build Cable Model
;;
;; Structure:
;;
;; (
;;   Panel ID
;;   Device ID
;;   Cable
;;   Loop Type
;; )
;;
;; ------------------------------------------------------------

(defun build-it-cable-model (raw-data / cables row) 

  (setq cables '())


  (foreach row raw-data 

    (setq cables (cons 
                   (list 
                     (nth 2 row) ;; Panel ID
                     (nth 1 row) ;; Device ID
                     (nth 5 row) ;; Cable
                     (nth 8 row) ;; Loop Type
                   )
                   cables
                 )
    )
  )


  (reverse cables)
)



;; ------------------------------------------------------------
;; Get all cables for a panel
;; ------------------------------------------------------------

(defun get-it-panel-cables (panel-id cable-data / result item) 

  (setq result '())


  (foreach item cable-data 

    (if (= panel-id (nth 0 item)) 

      (setq result (cons item result))
    )
  )


  (reverse result)
)



;; ------------------------------------------------------------
;; Get cable for one device
;; ------------------------------------------------------------

(defun get-it-device-cable (panel-id device-id cable-data / result item) 

  (setq result nil)


  (foreach item cable-data 

    (if 
      (and 
        (= panel-id (nth 0 item))
        (= device-id (nth 1 item))
      )

      (setq result (nth 2 item))
    )
  )


  result
)


;; ------------------------------------------------------------
;; Get cables for a device row
;;
;; Input:
;;   panel-id
;;   row-devices
;;   cable-data
;;
;; Returns:
;;   cable entries belonging to devices in this row
;;
;; ------------------------------------------------------------

(defun get-it-row-cables (panel-id row-devices cable-data / result device item)

  (setq result '())

  (foreach device row-devices

    (foreach item cable-data

      (if
        (and
          (= panel-id (nth 0 item))
          (= (nth 0 device) (nth 1 item))
        )

        (setq result
               (cons item result)
        )
      )
    )
  )

  (reverse result)
)


;; ------------------------------------------------------------
;; Count cable quantities
;;
;; Example:
;;
;; ((B . 2) (C . 3))
;;
;; ------------------------------------------------------------

(defun count-it-cables (cable-data / counts item parts cable qty part) 

  (setq counts '())

  (foreach item cable-data 

    ;; split cable field
    (setq parts (parseCSV (nth 2 item)))

    (foreach part parts 

      ;; default quantity
      (setq qty 1)

      ;; check for number prefix
      (if (numberp (read (substr part 1 1))) 

        (progn 
          (setq qty (atoi (substr part 1 1)))
          (setq cable (substr part 2))
        )

        (setq cable part)
      )


      ;; add to totals
      (if (assoc cable counts) 

        (setq counts (subst 
                       (cons cable (+ qty (cdr (assoc cable counts))))
                       (assoc cable counts)
                       counts
                     )
        )

        (setq counts (cons 
                       (cons cable qty)
                       counts
                     )
        )
      )
    )
  )

  counts
)



;; ------------------------------------------------------------
;; Format cable tag
;;
;; Example:
;; ((B . 2) (C . 3))
;;
;; Output:
;; 2B,3C
;;
;; ------------------------------------------------------------

(defun format-it-cable-tag (counts / result sorted item) 

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

(defun get-it-home-run-cables (panel-id cable-data / result item) 

  (setq result '())

  (foreach item cable-data 

    (if 
      (and 
        (= panel-id (nth 0 item))
        (= "Home_Run" (nth 3 item))
      )

      (setq result (cons item result))
    )
  )

  (reverse result)
)


