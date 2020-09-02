;; packages.el --- web-seaach layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;; Different approach to web search than built-in layer search-engine.
;;
;;; Code:

(defconst web-search-packages
	'(engine-mode))

(defun web-search/init-engine-mode ()
  (use-package engine-mode
    :config
    ;; todo: adapt engine bind key for evil
    ;; a/ igoogle-translates used in layer engine mode (which I don't want)
    (engine-mode t)

    (defengine amazon
      "http://www.amazon.de/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s"
      :keybinding "a")

    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "h")

    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")

    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
      :keybinding "i")

    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :keybinding "m")

    (defengine werstreamtes
      "http://www.werstreamt.es/filme-serien/?q=%s&action_results=suchen"
      :keybinding "s")

    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w")

    (defengine wikipedia-de
      "http://www.wikipedia.org/search-redirect.php?language=de&go=Go&search=%s"
      :keybinding "W")

    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y")

    ;; now that engines have been defined, bind them
    (spacemacs/declare-prefix web-search-prefix "web search")
    (dolist (km engine-mode-prefixed-map)
      (when (listp km)
        (let ((key (car km))
              (command (cdr km)))
          (spacemacs/set-leader-keys
            (concat web-search-prefix (char-to-string key))
            command))))
    ))
