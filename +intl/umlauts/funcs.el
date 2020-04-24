;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defun umlauts//remap-on (map &optional not-ue control)
  "Turn mapping of öäÖÄ to []{} on in the provided translation MAP.

If NOT-UE is nil, also remap ü to \\."
  (define-key map "ö" "[")
  (define-key map "Ö" "{")
  (define-key map "ä" "]")
  (define-key map "Ä" "}")
  (when control
    (define-key map (kbd "C-ö") #'umlauts//insert-oe)
    (define-key map (kbd "C-Ö") #'umlauts//insert-OE)
    (define-key map (kbd "C-ä") #'umlauts//insert-ae)
    (define-key map (kbd "C-Ä") #'umlauts//insert-AE))
  (unless not-ue
    (define-key map "ü" "\\")
    (when control
      (define-key map (kbd "C-ü") #'umlauts//insert-ue)
      (define-key map (kbd "C-Ü") #'umlauts//insert-UE))))

(defun umlauts//remap-off (map)
  "Turn mapping of öäÖÄü to []{}\\ off in the provided translation MAP."
  (define-key map "ö" nil)
  (define-key map "Ö" nil)
  (define-key map "ä" nil)
  (define-key map "Ä" nil)
  (define-key map "ü" nil)
  (define-key map "Ü" nil)
  (define-key map (kbd "C-ö") nil)
  (define-key map (kbd "C-Ö") nil)
  (define-key map (kbd "C-ä") nil)
  (define-key map (kbd "C-Ä") nil)
  (define-key map (kbd "C-ü") nil)
  (define-key map (kbd "C-Ü") nil))

(defun umlauts//insert-oe ()
  (interactive)
  (insert "ö"))

(defun umlauts//insert-OE ()
  (interactive)
  (insert "Ö"))

(defun umlauts//insert-ae ()
  (interactive)
  (insert "ä"))

(defun umlauts//insert-AE ()
  (interactive)
  (insert "Ä"))

(defun umlauts//insert-ue ()
  (interactive)
  (insert "ü"))

(defun umlauts//insert-UE ()
  (interactive)
  (insert "Ü"))

(defun umlauts//remap-char (char)
  "Map CHAR from öäÖÄü to []{}\\, otherwise return CHAR."
  (pcase char
    ('?ö ?\[)
    ('?Ö ?{)
    ('?ä ?\])
    ('?Ä ?})
    ('?a ?\ )
    (_ char)))

(defun umlauts//read-char-advice (orig-fun &rest args)
  "Instrument read-char to remap öäÖÄü to []{}\\."
  (umlauts//remap-char (apply orig-fun args)))

(defun umlauts//read-char-interactively (char)
  (interactive "c")
  (char-to-string char))

(defun umlauts//evil-surround-advice (orig-fun &optional char outer inner)
  "Instrument an evil-surround function to remap öäÖÄü to []{}\\."
  (unless char
    (setq char (call-interactively 'umlauts//read-char-interactively)))
  (setq char (umlauts//remap-char char))
  (apply orig-fun char outer inner nil))

(define-minor-mode global-umlauts-minor-mode
  "Remap öäÖÄü to []{}\\ everywhere."
  :init-value nil
  :global t
  :lighter " Ä"
  (if global-umlauts-minor-mode
      (progn
        (umlauts//remap-on evil-insert-state-map)
        (umlauts//remap-on evil-replace-state-map)
        (umlauts//remap-on evil-operator-state-map)
        (umlauts//remap-on evil-operator-shortcut-map)
        (umlauts//remap-on evil-ex-completion-map)
        (umlauts//remap-on evil-read-key-map)
        (umlauts//remap-on minibuffer-local-map)
        (umlauts//remap-on ivy-minibuffer-map)
        (advice-add 'read-char :around 'umlauts//read-char-advice)
        (advice-add 'evil-surround-change :around 'umlauts//evil-surround-advice)
        (advice-add 'evil-surround-delete :around 'umlauts//evil-surround-advice)
        (global-set-key (kbd "C-ö") 'umlauts//insert-oe)
        (global-set-key (kbd "C-Ö") 'umlauts//insert-OE)
        (global-set-key (kbd "C-ä") 'umlauts//insert-ae)
        (global-set-key (kbd "C-Ä") 'umlauts//insert-AE)
        (global-set-key (kbd "C-ü") 'umlauts//insert-ue)
        (global-set-key (kbd "C-Ü") 'umlauts//insert-UE)
        )
    (umlauts//remap-off evil-insert-state-map)
    (umlauts//remap-off evil-replace-state-map)
    (umlauts//remap-off evil-operator-state-map)
    (umlauts//remap-off evil-operator-shortcut-map)
    (umlauts//remap-off evil-ex-completion-map)
    (umlauts//remap-off evil-read-key-map)
    (umlauts//remap-off minibuffer-local-map)
    (umlauts//remap-off ivy-minibuffer-map)
    (advice-remove 'read-char 'umlauts//read-char-advice)
    (advice-remove 'evil-surround-change 'umlauts//evil-surround-advice)
    (advice-remove 'evil-surround-delete 'umlauts//evil-surround-advice)
    (global-unset-key (kbd "C-ö"))
    (global-unset-key (kbd "C-Ö"))
    (global-unset-key (kbd "C-ä"))
    (global-unset-key (kbd "C-ä"))
    (global-unset-key (kbd "C-ü"))
    (global-unset-key (kbd "C-Ü"))))

(define-minor-mode umlauts-minor-mode
  "Remap öäÖÄü to []{}\\ in the current buffer (to the extent possible)"
  :init-value nil
  :lighter " ä"
  ;; TODO can we do better than just insert state?
  (if umlauts-minor-mode
      (progn
        (umlauts//remap-on evil-insert-state-local-map nil t))
    (umlauts//remap-off evil-insert-state-local-map)))
