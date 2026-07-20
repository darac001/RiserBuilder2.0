;; ================================
;; Riser Builder Project Settings
;; ================================


;; Stores the current project folder path.
;; All project files are loaded relative to this folder.
(defun rb-set-project-folder (folder) 

  (setq *rb-project-folder* folder)

  (prompt 
    (strcat 
      "\nProject folder set to: "
      *rb-project-folder*
    )
  )

  (princ)
)


;; Returns the full path to the active system input CSV file.
(defun rb-get-input-file () 

  (strcat 
    *rb-project-folder*
    "\\ac_input.csv"
  )
)

;; Returns the full path to a library file.
;; Example:
;; ac_device_library.csv
;; ac_panel_library.csv
(defun rb-get-library-file (filename) 

  (strcat 
    *rb-project-folder*
    "\\libraries\\"
    filename
  )
)

;; Checks that all required project files exist.
;; Returns T if project is valid, NIL if files are missing.
(defun rb-validate-project (/ input-file device-lib panel-lib ok) 

  (setq ok T)

  ;; Check input CSV exists
  (setq input-file (rb-get-input-file))
  (if (not (findfile input-file)) 
    (progn 
      (prompt (strcat "\nERROR: Missing input file: " input-file))
      (setq ok NIL)
    )
  )

  ;; Check device library exists
  (setq device-lib (rb-get-library-file "ac_device_library.csv"))
  (if (not (findfile device-lib)) 
    (progn 
      (prompt (strcat "\nERROR: Missing device library: " device-lib))
      (setq ok NIL)
    )
  )

  ;; Check panel library exists
  (setq panel-lib (rb-get-library-file "ac_panel_library.csv"))
  (if (not (findfile panel-lib)) 
    (progn 
      (prompt (strcat "\nERROR: Missing panel library: " panel-lib))
      (setq ok NIL)
    )
  )

  ok
)