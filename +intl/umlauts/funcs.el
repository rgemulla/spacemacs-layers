;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defun umlauts--remap-on (map)
  """Turn mapping of öäÖÄ to []{} on in the provided translation
  MAP."""
  (define-key map "ö" "[")
  (define-key map "Ö" "{")
  (define-key map "ä" "]")
  (define-key map "Ä" "}"))

(defun umlauts--remap-off (map)
  """Turn mapping of öäÖÄ to []{} off in the provided translation
  MAP."""
  (define-key map "ö" nil)
  (define-key map "Ö" nil)
  (define-key map "ä" nil)
  (define-key map "Ä" nil))

(defun umlauts--insert-oe ()
  (interactive)
  (insert "ö"))

(defun umlauts--insert-OE ()
  (interactive)
  (insert "Ö"))

(defun umlauts--insert-ae ()
  (interactive)
  (insert "ä"))

(defun umlauts--insert-AE ()
  (interactive)
  (insert "Ä"))

(defun umlauts--insert-ue ()
  (interactive)
  (insert "ü"))

(defun umlauts--insert-UE ()
  (interactive)
  (insert "Ü"))

(define-minor-mode umlauts-minor-mode
  "Remap öäÖÄ to []{} everywhere."
  :init-value nil
  :global t
  :lighter " ä"
  (if umlauts-minor-mode
      (progn
        (umlauts--remap-on key-translation-map)
        (global-set-key (kbd "C-ö") 'umlauts--insert-oe)
        (global-set-key (kbd "C-Ö") 'umlauts--insert-OE)
        (global-set-key (kbd "C-ä") 'umlauts--insert-ae)
        (global-set-key (kbd "C-Ä") 'umlauts--insert-AE)
        (global-set-key (kbd "C-ü") 'umlauts--insert-ue)
        (global-set-key (kbd "C-Ü") 'umlauts--insert-UE))
    (umlauts--remap-off key-translation-map)
    (global-unset-key (kbd "C-ö"))
    (global-unset-key (kbd "C-Ö"))
    (global-unset-key (kbd "C-ä"))
    (global-unset-key (kbd "C-ä"))
    (global-unset-key (kbd "C-ü"))
    (global-unset-key (kbd "C-Ü"))))
