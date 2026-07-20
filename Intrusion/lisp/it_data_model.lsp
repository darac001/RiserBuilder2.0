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