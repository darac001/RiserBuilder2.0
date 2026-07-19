;; ================================
;; Riser Builder Main Loader
;; ================================

;; Store the folder location where all Riser Builder LISP files are located.
;; If not already set, ask the user for the LISP folder path.
(if (not *rb-lisp-path*)

  (setq *rb-lisp-path*
    (getstring T "\nEnter Riser Builder LISP folder path: ")
  )

)

;; Load common project settings
(load
  (strcat *rb-lisp-path* "\\project_settings.lsp")
)


;; Load Access Control settings
(load
  (strcat *rb-lisp-path* "\\ac_layout_settings.lsp")
)


;; Load Access Control Parser
(load
  (strcat *rb-lisp-path* "\\ac_input_parser.lsp")
)


;; Load Access Control Data Model
(load
  (strcat *rb-lisp-path* "\\ac_data_model.lsp")
)


;; Load Access Control Layout Engine
(load
  (strcat *rb-lisp-path* "\\ac_layout_engine.lsp")
)


;; Load Access Control Libraries
(load
  (strcat *rb-lisp-path* "\\ac_libraries.lsp")
)





(princ "\nRiser Builder loaded.")
(princ)


;; ================================
;; Initialize Project
;; ================================
;; Sets project folder and loads project files.
;; Run this once for each project.

(defun c:RB-INIT (/ folder)

  (prompt "\n--- Riser Builder Init ---")

  (setq folder (getstring "\nEnter project folder path: "))

  (rb-set-project-folder folder)

  ;; VALIDATE FIRST
  (if (rb-validate-project)
    (progn
      (rb-load-libraries)
      (rb-load-input)
      (prompt "\nProject ready. Enter RB-GENERATE to generate a riser diagram.")
    )
    (prompt "\nInitialization failed. Fix errors above.")
  )

  (princ)
)


;; ================================
;; Generate Riser Diagram
;; ================================
;; Reloads project data and creates the drawing.

(defun c:RB-GENERATE (/)

  (prompt "\n--- Riser Builder Generate ---")

  ;; 1. Check if project folder is set
  (if (not *rb-project-folder*)
    (progn
      (prompt "\nERROR: Project folder not set. Run RB-INIT first.")
    )
    
    ;; else continue
    (progn

      ;; 2. Validate project structure
      (if (rb-validate-project)
        (progn

          ;; 3. Load everything fresh
          (rb-load-libraries)
          (rb-load-input)

          ;; 4. Run main drawing function
          (DRAW_RISER)

          (prompt "\nRiser generated successfully.")

        )
        (prompt "\nGeneration failed. Fix project errors.")
      )

    )
  )

  (princ)
)


