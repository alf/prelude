;;; setup-ui.el --- My UI customizations
;;
;;; Commentary:

;; The defaults are never enough

;;; Code:

(setq visible-bell t)

(if (> (display-pixel-width) 1440)
    (set-face-attribute 'default nil :height 200)
  (set-face-attribute 'default nil :height 140))

(require 'helm)
(setq helm-split-window-default-side 'right)
(setq helm-split-window-in-side-p nil)

;; Prefer eww when opening urls
(setq browse-url-browser-function 'eww-browse-url)

(provide 'setup-ui)
;;; setup-ui.el ends here