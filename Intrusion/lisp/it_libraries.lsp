;; ============================================================
;; Intrusion Riser Libraries
;;
;; Loads device and panel library CSV files.
;;
;; ============================================================


(setq *it-device-library* nil)
(setq *it-panel-library* nil)

; temp
(setq *it-project-folder* "C:/Users/darko/Desktop/Athabasca/COMP495/Project2.0/Intrusion")

;; ------------------------------------------------------------
;; Load Device Library
;; ------------------------------------------------------------

(defun it-load-device-library (/ file rows) 

  (setq file (strcat 
               *it-project-folder*
               "\\libraries\\it_device_library.csv"
             )
  )


  (setq rows (parse-csv 
               (read-csv file)
             )
  )


  (setq *it-device-library* rows)


  (prompt "\nIntrusion device library loaded.")
)



;; ------------------------------------------------------------
;; Load Panel Library
;; ------------------------------------------------------------

(defun it-load-panel-library (/ file rows) 

  (setq file (strcat 
               *it-project-folder*
               "\\libraries\\it_panel_library.csv"
             )
  )


  (setq rows (parse-csv 
               (read-csv file)
             )
  )


  (setq *it-panel-library* rows)


  (prompt "\nIntrusion panel library loaded.")
)



;; ------------------------------------------------------------
;; Find device block
;;
;; Input:
;;   MD
;;
;; Output:
;;   IDS_MD_Motion_Detector
;;
;; ------------------------------------------------------------

(defun get-it-device-block (acronym / item) 

  (setq item nil)


  (foreach row *it-device-library* 

    (if (= acronym (nth 0 row)) 

      (setq item (nth 1 row))
    )
  )


  item
)



;; ------------------------------------------------------------
;; Find panel block
;;
;; Input:
;;   IDCP
;;
;; Output:
;;   IDCP_FULL
;;
;; ------------------------------------------------------------

(defun get-it-panel-block (panel-type / item) 

  (setq item nil)


  (foreach row *it-panel-library* 

    (if (= panel-type (nth 0 row)) 

      (setq item (nth 1 row))
    )
  )


  item
)



;; ------------------------------------------------------------
;; Check if panel requires PSU
;; ------------------------------------------------------------

(defun it-panel-requires-psu (panel-type / item) 

  (setq item nil)


  (foreach row *it-panel-library* 

    (if (= panel-type (nth 0 row)) 

      (setq item (nth 2 row))
    )
  )


  (= item "YES")
)



;; ------------------------------------------------------------
;; Get PSU block
;; ------------------------------------------------------------

(defun get-it-panel-ps-block (panel-type / item) 

  (setq item nil)


  (foreach row *it-panel-library* 

    (if (= panel-type (nth 0 row)) 

      (setq item (nth 3 row))
    )
  )


  item
)