;; ============================================================
;; Intrusion Riser Main Controller
;;
;; Responsibilities:
;; - Load intrusion riser modules
;; - Initialize project data
;; - Build data models
;; - Start drawing
;;
;; ============================================================



;; ------------------------------------------------------------
;; Global Project Data
;; ------------------------------------------------------------
(vl-load-com)
(prompt "\n*** LOADING CURRENT IT_MAIN.LSP ***")
(setq *it-model* nil)
(setq *it-cable-data* nil)
(setq *it-registry* "HKEY_CURRENT_USER\\Software\\RiserBuilder")


(defun it-debug-log (msg / file) 
  (setq file (open "C:/temp/it_debug.txt" "a"))
  (write-line msg file)
  (close file)
)


;; ------------------------------------------------------------
;; Load All Intrusion Riser Files
;; ------------------------------------------------------------

(defun IT-LOAD (/) 

  ;; Set LISP Path (only once per session)
  (if (not *it-lisp-path*) 

    (setq *it-lisp-path* (vl-registry-read 
                           *it-registry*
                           "LispPath"
                         )
    )
  )


  (if (not *it-lisp-path*) 
    (setq *it-lisp-path* (getstring T "\nEnter Riser Builder LISP folder path: "))
  )


  (if *it-lisp-path* 

    (vl-registry-write 
      *it-registry*
      "LispPath"
      *it-lisp-path*
    )
  )

  (if (not *it-lisp-path*) 
    (progn 
      (prompt "\nLISP folder selection cancelled.")
      (exit)
    )
  )

  (prompt "\n--- Riser Modules ---")


  (load (strcat *it-lisp-path* "\\it_input_parser.lsp"))
  (load (strcat *it-lisp-path* "\\it_data_model.lsp"))
  (load (strcat *it-lisp-path* "\\it_data_model_helpers.lsp"))
  (load (strcat *it-lisp-path* "\\it_layout_settings.lsp"))
  (load (strcat *it-lisp-path* "\\it_libraries.lsp"))
  (load (strcat *it-lisp-path* "\\it_layout_engine.lsp"))

  (princ)
)



(defun IT-SELECT-SYSTEM (/ system valid) 
  (setq valid nil)

  (while (not valid) 
    (setq system (getstring "\nSelect system (ACS / IDS / CCTV): "))
    (setq system (strcase system))

    (if (member system '("ACS" "IDS" "CCTV")) 
      (setq valid T)
      (prompt "\nInvalid input. Try again.")
    )
  )

  ;; pass to settings
  (it-set-system system)
)


(defun it-set-project-folder (folder) 
  (setq *it-project-folder* folder)

  ;; normalize (optional but smart)
  (if (/= (substr folder (strlen folder) 1) "\\") 
    (setq *it-project-folder* (strcat folder "\\"))
  )

  (vl-registry-write 
    *it-registry*
    "ProjectPath"
    *it-project-folder*
  )
)


(defun it-validate-project (/ ok input-file library-file) 

  (setq ok T)

  (setq input-file (strcat *it-project-folder* "input.csv"))
  (if (not (findfile input-file)) 
    (progn 
      (prompt "\nERROR: input.csv not found.")
      (setq ok nil)
    )
  )

  (setq library-file (strcat *it-lisp-path* "\\..\\libraries\\panel_library.csv"))
  (if (not (findfile library-file)) 
    (progn 
      (prompt "\nERROR: panel library not found.")
      (setq ok nil)
    )
  )

  ok
)



(defun it-set-system (system /) 
  ;; normalize
  (setq system (strcase system))

  ;; validate
  (if (not (member system '("ACS" "IDS" "CCTV"))) 
    (progn 
      (prompt "\nERROR: Invalid system type.")
      (exit)
    )
  )

  ;; store globally
  (setq *it-system* system)

  ;; set layers dynamically
  (setq *it-layer-device* (strcat "E-SERT-" system))
  (setq *it-layer-text* (strcat "E-SERT-" system "-TEXT"))

  (prompt (strcat "\nSystem set to: " system))
)



;; Update Riser Builder Paths
;; Update Riser Builder Paths
(defun c:IT-UPDATE-PATHS (/ project-folder) 

  (prompt "\n--- Update Riser Builder Paths ---")


  ;; ---------------------------------
  ;; Update LISP folder
  ;; ---------------------------------

  (setq *it-lisp-path* (getstring T "\nEnter Riser Builder LISP folder path: "))


  ;; Validate LISP folder
  (while (not (vl-file-directory-p *it-lisp-path*)) 

    (prompt "\nInvalid LISP folder.")

    (setq *it-lisp-path* (getstring T "\nEnter Riser Builder LISP folder path: "))
  )


  ;; Save LISP path
  (vl-registry-write 
    *it-registry*
    "LispPath"
    *it-lisp-path*
  )


  ;; ---------------------------------
  ;; Update Project folder
  ;; ---------------------------------

  (setq project-folder (getstring T "\nEnter project folder path: "))


  ;; Validate project folder
  (while (not (vl-file-directory-p project-folder)) 

    (prompt "\nInvalid project folder.")

    (setq project-folder (getstring T "\nEnter project folder path: "))
  )


  ;; Store project path
  (it-set-project-folder project-folder)


  ;; ---------------------------------
  ;; Clear old project data
  ;; ---------------------------------

  (setq *it-model* nil)
  (setq *it-cable-data* nil)
  (setq *it-input-data* nil)
  (setq *it-panel-library* nil)


  (prompt "\nPaths updated successfully.")
  (prompt "\nRun IT-START again.")


  (princ)
)

;; ------------------------------------------------------------
;; Initialize Intrusion Project
;;
;; Reads CSV and builds models
;; ------------------------------------------------------------

(defun IT-INIT (/ folder) 

  ;; Clear previous project data
  (setq *it-model* nil)
  (setq *it-cable-data* nil)
  (setq *it-input-data* nil)
  (setq *it-panel-library* nil)


  (prompt "\n--- Initializing Project ---")


  ;; Ask for project folder

  ; (setq folder (getstring T "\nEnter project folder path: "))

  (setq folder (vl-registry-read 
                 *it-registry*
                 "ProjectPath"
               )
  )

  (if (not folder) 
    (setq folder (getstring T "\nEnter project folder path: "))
  )


  (if folder 
    (it-set-project-folder folder)
  )


  (if (not folder) 
    (progn 
      (prompt "\nProject selection cancelled.")
      (exit)
    )
  )

  ;; Validate before continuing
  (if (it-validate-project) 
    (progn 
      (it-load-panel-library)
      (it-load-input)

      ;; Build models
      (setq *it-model* (build-it-data-model *it-input-data*))
      (setq *it-cable-data* (build-it-cable-model *it-input-data*))

      (prompt "\nProject initialized successfully.")
      T
    )
    (progn 
      (prompt "\nInitialization failed. Fix errors above.")
      nil
    )
  )

  (princ)
)

(defun c:IT-INIT (/) 

  (if (IT-INIT) 

    (prompt "\nProject re-initialized successfully.")

    (prompt "\nInitialization failed.")
  )

  (princ)
)

;; ------------------------------------------------------------
;; Convenience Command
;;
;; Load + Initialize + Draw
;; ------------------------------------------------------------

(defun c:IT-START (/) 


  ;; Clear debug log
  (setq f (open "C:/temp/it_debug.txt" "w"))
  (close f)

  (IT-SELECT-SYSTEM)

  (IT-LOAD)


  (if (IT-INIT) 

    (IT-DRAW-RISER 
      *it-model*
      *it-cable-data*
    )

    (prompt "\nRiser generation cancelled.")
  )

  (princ)
)




;; ------------------------------------------------------------
;; Display Riser Builder Configuration
;; ------------------------------------------------------------

(defun c:IT-CHECK (/ input-file library-file) 

  ;; Build paths only if variables exist

  (if *it-project-folder* 
    (setq input-file (strcat *it-project-folder* "input.csv"))
    (setq input-file nil)
  )

  (if *it-lisp-path* 
    (setq library-file (strcat *it-lisp-path* 
                               "\\..\\libraries\\panel_library.csv"
                       )
    )
    (setq library-file nil)
  )


  (prompt "\n==============================")
  (prompt "\n Riser Builder Configuration")
  (prompt "\n==============================")

  ;; System
  (if *it-system* 
    (prompt (strcat "\nSystem: " *it-system*))
    (prompt "\nSystem: NOT SET")
  )


  ;; LISP path
  (if *it-lisp-path* 
    (prompt (strcat "\nLISP Folder: " *it-lisp-path*))
    (prompt "\nLISP Folder: NOT SET")
  )


  ;; Project path
  (if *it-project-folder* 
    (prompt (strcat "\nProject Folder: " *it-project-folder*))
    (prompt "\nProject Folder: NOT SET")
  )


  ;; Input CSV
  (if input-file 
    (if (findfile input-file) 
      (prompt "\nInput CSV: OK")
      (prompt "\nInput CSV: MISSING")
    )
    (prompt "\nInput CSV: PROJECT NOT SET")
  )


  ;; Panel library
  (if library-file 
    (if (findfile library-file) 
      (prompt "\nPanel Library: OK")
      (prompt "\nPanel Library: MISSING")
    )
    (prompt "\nPanel Library: LISP PATH NOT SET")
  )


  (prompt "\n==============================")

  (princ)
)

  ;; ------------------------------------------------------------
  ;; Reset Current Riser Session Data
  ;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; Clear Saved Riser Builder Paths
;;
;; Removes registry saved paths
;; Forces setup again on next IT-START
;; ------------------------------------------------------------

(defun c:IT-RESET-PATHS (/) 

  ;; Remove saved paths from registry

  (vl-registry-delete 
    *it-registry*
    "LispPath"
  )

  (vl-registry-delete 
    *it-registry*
    "ProjectPath"
  )


  ;; Clear current session variables

  (setq *it-lisp-path* nil)
  (setq *it-project-folder* nil)


  (prompt "\nRiser Builder saved paths cleared.")
  (prompt "\nNext IT-START will ask for paths again.")


  (princ)
)


  ;; ------------------------------------------------------------
  ;; Draw Current Riser Model
  ;;
  ;; Uses already initialized project data
  ;; ------------------------------------------------------------
(defun c:IT-DRAW (/) 

  (if (and *it-model* *it-cable-data*) 

    (progn 

      (prompt "\n--- Drawing Riser ---")

      (IT-DRAW-RISER 
        *it-model*
        *it-cable-data*
      )
    )

    (prompt "\nERROR: Project not initialized. Run IT-START first.")
  )

  (princ)
)