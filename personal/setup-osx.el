;;; setup-osx.el --- OSX specific settings.
;;
;;; Commentary:

;; Some OSX specific stuff.

;;; Code:

;; I use CAPSLOCK for typing accented characters, this works system
;; wide, so Emacs needs to fall in line here.
(setq ns-alternate-modifier 'none)

;; I prefer the command keys for meta, and a symetrical keyboard
;; layout so I can alternate which hands holds the control keys
(setq ns-command-modifier 'meta)

(provide 'setup-osx)
;;; setup-osx.el ends here
