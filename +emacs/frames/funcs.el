;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defun frames/move-selected-frame-to-screen-bottom (ARG)
  (interactive (list (if current-prefix-arg
                         (* (frame-char-height) (prefix-numeric-value current-prefix-arg))
                       0)))
  (move-frame-to-screen-bottom ARG (selected-frame)))

(defun frames/move-selected-frame-to-screen-top (ARG)
  (interactive (list (if current-prefix-arg
                         (* (frame-char-height) (prefix-numeric-value current-prefix-arg))
                       0)))
  (move-frame-to-screen-top ARG (selected-frame)))

(defun frames/move-selected-frame-to-screen-left (ARG)
    (interactive (list (if current-prefix-arg
                           (* (frame-char-width) (prefix-numeric-value current-prefix-arg))
                         0)))
    (move-frame-to-screen-left ARG (selected-frame)))

(defun frames/move-selected-frame-to-screen-right (ARG)
  (interactive (list (if current-prefix-arg
                         (* (frame-char-width) (prefix-numeric-value current-prefix-arg))
                       0)))
  (move-frame-to-screen-right ARG (selected-frame)))

(defun frames/frame-enlarge-and-split-horizontally ()
  "Enlarge frame width by width of its widest window and create a
new window in the newly created space."
  (interactive)
  (let ((added-width 0))
    (dolist (w (window-list (selected-frame)))
      (let ((width (window-total-width w)))
        (when (> width added-width)
          (setq added-width width))))

    (set-frame-width nil (+ (frame-total-cols) added-width))
    (let ((new-window (split-window (frame-root-window) (- added-width) 'right)))
      (with-selected-window new-window
        (switch-to-buffer (other-buffer))))))

(defun frames/frame-shrink-and-delete-horizontally ()
  "Shrink frame width by width of its thinnest right-most window
and delete all right-most windows of this width.

Roughly the inverse of
`frames/frame-enlarge-and-split-horizontally'.

If the thinnest-rightmost window covers the whole frame (i.e.,
there is no vertical spit), halves the frame width."
  (interactive)
  (let ((max-right -1)
        (removed-width 0))
    (dolist (w (window-list (selected-frame)))
      (let* ((width (window-total-width w))
             (edges (window-edges w))
             (right (nth 2 edges)))
        (if (> right max-right)
            (setq max-right right
                  removed-width width)
          (when (< width removed-width)
            (setq removed-width width)))))

    (if (= (frame-total-cols) removed-width)
        (set-frame-width nil (/ (frame-total-cols) 2))
      (dolist (w (window-list (selected-frame)))
        (let* ((width (window-total-width w))
               (edges (window-edges w))
               (right (nth 2 edges)))
          (when (and (= right max-right)
                     (= width removed-width))
            (delete-window w))))
      (set-frame-width nil (- (frame-total-cols) removed-width)))))
