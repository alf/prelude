;;; setup-ui.el --- My UI customizations
;;
;;; Commentary:

;; The defaults are never enough

;;; Code:

(setq visible-bell t)

(if (> (display-pixel-width) 1440)
    (set-face-attribute 'default nil :height 200)
  (set-face-attribute 'default nil :height 140))

(provide 'setup-ui)
;;; setup-ui.el ends here
