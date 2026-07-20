;; ============================================================
;; Access Control Riser Builder Layout Settings
;;
;; Contains drawing layout parameters used by the layout engine.
;; Values are based on AutoCAD drawing units.
;;
;; ============================================================

;; Door layout settings

;; Maximum number of doors per horizontal riser row
(setq *rb-door-limit* 8)

;; Distance between doors on horizontal riser
(setq *rb-door-spacing* 1.5)

;; Vertical distance between door rows
(setq *rb-door-row-gap* 2)


;; Panel settings

;; Panel block dimensions (drawing units - inches)
(setq *rb-panel-width* 1.375)
(setq *rb-panel-height* 1.4375)

;; External PSU horizontal offset from panel
(setq *rb-ps-offset-x* 0.25)

;; Distance between stacked panels
(setq *rb-panel-spacing* 7) 

;; Half panel width used for connection calculations
(setq *rb-panel-half-width* 0.6875) 


;; Device layout settings

;; Device block dimensions
(setq *rb-device-width* 0.1875)
(setq *rb-device-height* 0.1875)

;; Distance between two device columns
(setq *rb-device-column-gap* 0.3)

;; Distance between stacked devices vertically
(setq *rb-device-row-gap* 0.28)



;;Door brach settings

;; vertical length of the branch line
(setq *rb-door-drop* 1.0)


;; Door label settings

(setq *rb-door-id-offset* 0.1)

(setq *rb-door-id-text-style* "MtXpl_Arial_Narrow")

(setq *rb-door-id-text-height* 0.09375)

(setq *rb-door-id-x-offset* 0.15)



;; General row spacing

;; Distance between ACP rows
(setq *rb-row-spacing* 3.0)

(setq *rb-row-gap* 0.5)

(setq *rb-row-x-step* 0.3)


;; Cable leader / tag settings

(setq *rb-leader-length* 0.5)

(setq *rb-leader-text-height* 0.125)

(setq *rb-leader-offset-x* 0.3) ; for vertical drops

(setq *rb-leader-offset-y* 0.4) ; for horizontal trunk

(setq *rb-wire-tag-offset* -1)


;; Row transition chamfer

(setq *rb-row-chamfer-length* 0.3)

(setq *rb-row-chamfer-height* 0.3)



