;;; setup-keybindings -- Global key bindings
;;; Commentary:

;; This package contains my global keybindings that are not mode
;; specific. That is, my global keybindings for helm, projectile and
;; org-mode are defined in their respective setup files.

;;; Code:

;; Toggle features easily with: C-x t <key>
;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'alf/toggle-map)
(define-key ctl-x-map "t" 'alf/toggle-map)
(define-key alf/toggle-map "c" 'column-number-mode)
(define-key alf/toggle-map "d" 'toggle-debug-on-error)
(define-key alf/toggle-map "e" 'toggle-debug-on-error)
(define-key alf/toggle-map "f" 'auto-fill-mode)
(define-key alf/toggle-map "l" 'toggle-truncate-lines)
(define-key alf/toggle-map "q" 'toggle-debug-on-quit)
(define-key alf/toggle-map "n" #'narrow-or-widen-dwim)
(define-key alf/toggle-map (kbd "RET") 'toggle-frame-fullscreen)

;;; "C-x t n" to enter org src blocks, and "C-x C-s" to exit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))


(define-key ctl-x-map (kbd "C-z") 'magit-status)

;;; My very own prefix key
(define-prefix-command 'alf/ctl-z-map)
(define-key global-map (kbd "C-z") 'alf/ctl-z-map)
(define-key alf/ctl-z-map "z" 'ace-jump-mode)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
