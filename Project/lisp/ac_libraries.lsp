;; ============================================================
;; Access Control Library Manager
;;
;; Loads device and panel block libraries from CSV files and
;; provides lookup functions used by the layout engine.
;;
;; ============================================================

(setq *device-library* nil)
(setq *panel-library* nil)


;; Splits a CSV line into a list of values.
;;
;; Used for library CSV files where fields do not contain
;; embedded commas.
(defun split-csv-line (line / result pos) 
  (setq result '())
  (while (setq pos (vl-string-search "," line)) 
    (setq result (cons (substr line 1 pos) result))
    (setq line (substr line (+ pos 2)))
  )
  (reverse (cons line result))
)


;; Loads device block information from CSV library.
;;
;; Updates:
;;   *device-library*
;;
;; CSV format:
;;   Acronym,Block Name,Description

(defun load-device-library (filepath / file line data) 
  (setq *device-library* '())

  (setq file (open filepath "r"))

  (if file 
    (progn 
      ;; skip header
      (read-line file)

      (while (setq line (read-line file)) 
        (setq data (split-csv-line line))

        (setq *device-library* (cons 
                                 (list 
                                   (nth 0 data) ;; Acronym
                                   (nth 1 data) ;; Block Name
                                   (nth 2 data) ;; Description
                                 )
                                 *device-library*
                               )
        )
      )

      (close file)
    )
    (prompt "\nError: Could not open device library file.")
  )
)

;; Loads panel block information from CSV library.
;;
;; Updates:
;;   *panel-library*
;;
;; CSV format:
;;   Panel Type,Block Name,External PSU Required,PS Block Name

(defun load-panel-library (filepath / file line data) 
  (setq *panel-library* '())

  (setq file (open filepath "r"))

  (if file 
    (progn 
      ;; skip header
      (read-line file)

      (while (setq line (read-line file)) 
        (setq data (split-csv-line line))

        (setq *panel-library* (cons 
                                (list 
                                  (nth 0 data) ;; Panel Type
                                  (nth 1 data) ;; Block Name
                                  (nth 2 data) ;; External PSU Required
                                  (nth 3 data) ;; PS Block Name
                                )
                                *panel-library*
                              )
        )
      )

      (close file)
    )
    (prompt "\nError: Could not open panel library file.")
  )
)

;; Finds AutoCAD block name for a device acronym.
;;
;; Input:
;;   acronym - Device code (CR, DC, ES, etc.)
;;
;; Returns:
;;   Block name or NIL if not found.

(defun get-device-block (acronym / result) 
  (setq result (vl-some 
                 '(lambda (item) 
                    (if (= (strcase acronym) (strcase (nth 0 item))) 
                      (nth 1 item)
                    )
                  )
                 *device-library*
               )
  )

  (if result 
    result
    (progn 
      (prompt (strcat "\nWarning: Device not found: " acronym))
      nil
    )
  )
)

;; Finds AutoCAD block name for a panel type.
(defun get-panel-block (panel-type / result) 
  (setq result (vl-some 
                 '(lambda (item) 
                    (if (= (strcase panel-type) (strcase (nth 0 item))) 
                      (nth 1 item)
                    )
                  )
                 *panel-library*
               )
  )

  result
)

;; Checks if a panel requires an external power supply.
;;
;; Returns:
;;   T   - External PSU required
;;   NIL - No external PSU required

(defun panel-requires-psu (panel-type / result) 
  (setq result (vl-some 
                 '(lambda (item) 
                    (if (= (strcase panel-type) (strcase (nth 0 item))) 
                      (nth 2 item)
                    )
                  )
                 *panel-library*
               )
  )

  (if (= (strcase result) "YES") 
    T
    NIL
  )
)


;; Loads all Access Control libraries.
;;
;; Uses project library path settings.
;;
;; Loads:
;;   ac_device_library.csv
;;   ac_panel_library.csv

(defun rb-load-libraries () 

  (load-device-library 
    (rb-get-library-file "ac_device_library.csv")
  )

  (load-panel-library 
    (rb-get-library-file "ac_panel_library.csv")
  )

  (prompt "\nLibraries loaded successfully.")
  (princ)
)



;; Prints loaded device library contents.
;;
;; Used for testing/debugging.

(defun c:print-device-library () 
  (princ "\n--- Device Library ---")
  (foreach item *device-library* 
    (princ 
      (strcat 
        "\nAcronym: "
        (nth 0 item)
        " | Block: "
        (nth 1 item)
        " | Desc: "
        (nth 2 item)
      )
    )
  )
  (princ)
)

;; Prints loaded panel library contents.
;;
;; Used for testing/debugging.

(defun c:print-panel-library () 
  (princ "\n--- Panel Library ---")
  (foreach item *panel-library* 
    (princ 
      (strcat 
        "\nType: "
        (nth 0 item)
        " | Block: "
        (nth 1 item)
        " | External PSU: "
        (nth 2 item)
        " | PS Block: "
        (if (nth 3 item) 
          (nth 3 item)
          "N/A"
        )
      )
    )
  )
  (princ)
)

;; Gets power supply block associated with a panel type.
;;
;; Returns:
;;   PSU block name or NIL

(defun get-panel-ps-block (panel-type / result) 

  (setq result (vl-some 
                 '(lambda (item) 
                    (if 
                      (= (strcase panel-type) 
                         (strcase (nth 0 item))
                      )
                      (nth 3 item)
                    )
                  )
                 *panel-library*
               )
  )

  result
)

