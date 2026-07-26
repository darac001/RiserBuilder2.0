;; ============================================================
;; Intrusion Riser Layout Settings
;; ============================================================


;; Panel dimensions

(setq *it-panel-width* 1.375)

(setq *it-panel-height* 1.4375)


;; Vertical spacing between panels

(setq *it-panel-spacing* 7.0)


;;horizontal step
(setq *it-riser-offset-step* 0.1875)

;; Device spacing

(setq *it-device-spacing* 1.5)



;; Maximum devices per horizontal row

(setq *it-device-limit* 8)






;; ------------------------------------------------------------
;; Home Run (device drop) settings
;; ------------------------------------------------------------

;; vertical drop length (panel → device)
(setq *it-device-drop* 1.0)


;; horizontal offset from panel to first device
(setq *it-device-start-offset* 2.0)


;; ------------------------------------------------------------
;; Row spacing
;; ------------------------------------------------------------

(setq *it-row-spacing* 2.5)


;; ------------------------------------------------------------
;; Label settings
;; ------------------------------------------------------------

(setq *it-device-id-offset* 0.1)

(setq *it-device-id-text-style* "MtXpl_Arial_Narrow")

(setq *it-device-id-text-height* 0.09375)

(setq *it-device-id-x-offset* 0.15)


;; ------------------------------------------------------------
;; Leader / wire tag (future)
;; ------------------------------------------------------------

(setq *it-leader-length* 0.5)

(setq *it-leader-text-height* 0.125)

(setq *it-leader-offset-x* 0.3)

(setq *it-leader-offset-y* 0.4)

(setq *it-wire-tag-offset* -1)


;; Cable leader settings

(setq *it-wire-tag-offset* 0.25)

(setq *it-leader-text-height* 0.125)





;; Devices

(setq *it-device-width* 0.21702924) 

(setq *it-device-height* 0.21702924)


;; Daisy chain settings


;; ============================================================
;; Daisy Chain Settings
;; ============================================================

;; Panel to first device cable length
(setq *it-daisy-first-trunk-length* 1.88998201)


;; Device to device cable length
(setq *it-daisy-device-spacing* 1.0)


;; ============================================================
;; Layer Settings
;; ============================================================

(setq *it-layer-cable* "E-SEC-WIRE")

; (setq *it-layer-device* "E-SERT-IDS")

; (setq *it-layer-text* "E-SERT-IDS-TEXT")


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