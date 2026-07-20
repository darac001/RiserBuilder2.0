;; ============================================================
;; Intrusion Library Test
;;
;; Tests:
;; - Device library loading
;; - Panel library loading
;; - Device block lookup
;; - Panel block lookup
;; - PSU checks
;;
;; ============================================================


(defun c:TEST-IT-LIBRARIES (/)

  (prompt "\n--- Testing Intrusion Libraries ---")


  ;; Load libraries

  (it-load-device-library)

  (it-load-panel-library)



  ;; --------------------------------
  ;; Test Device Lookup
  ;; --------------------------------

  (prompt "\n\nTesting Device Lookup:")


  (prompt "\nMD block: ")

  (princ
    (get-it-device-block "MD")
  )


  (prompt "\nKP block: ")

  (princ
    (get-it-device-block "KP")
  )


  (prompt "\nDA block: ")

  (princ
    (get-it-device-block "DA")
  )



  ;; --------------------------------
  ;; Test Panel Lookup
  ;; --------------------------------

  (prompt "\n\nTesting Panel Lookup:")


  (prompt "\nIDCP block: ")

  (princ
    (get-it-panel-block "IDCP")
  )


  (prompt "\nZE block: ")

  (princ
    (get-it-panel-block "ZE")
  )



  ;; --------------------------------
  ;; Test PSU
  ;; --------------------------------

  (prompt "\n\nTesting PSU Requirements:")


  (prompt "\nIDCP requires PSU: ")

  (princ
    (it-panel-requires-psu "IDCP")
  )


  (prompt "\nIDCP_PS requires PSU: ")

  (princ
    (it-panel-requires-psu "IDCP_PS")
  )


  ;; --------------------------------
  ;; Test PSU Block
  ;; --------------------------------

  (prompt "\n\nTesting PSU Block:")


  (prompt "\nIDCP_PS PSU Block: ")

  (princ
    (get-it-panel-ps-block "IDCP_PS")
  )


  (prompt "\nZE_PS PSU Block: ")

  (princ
    (get-it-panel-ps-block "ZE_PS")
  )



  (prompt "\n\nLibrary test complete.")

  (princ)

)