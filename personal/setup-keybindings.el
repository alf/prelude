;;; setup-keybindings -- Global key bindings
;;; Commentary:

;; This package contains my global keybindings.

;;; Code:

;; Switch easily between frames
(global-set-key (kbd "M-`") 'other-frame)

(require 'projectile)
(define-key projectile-command-map (kbd "b") 'projectile-switch-to-buffer)
(define-key projectile-command-map (kbd "d") 'helm-projectile-find-dir)
(define-key projectile-command-map (kbd "e") 'helm-projectile-recentf)
(define-key projectile-command-map (kbd "f") 'helm-projectile-find-file)
(define-key projectile-command-map (kbd "F") 'helm-projectile-find-file-in-known-projects)
(define-key projectile-command-map (kbd "g") 'helm-projectile-find-file-dwim)
(define-key projectile-command-map (kbd "s a") 'projectile-ag)
(define-key projectile-command-map (kbd "s g") 'projectile-grep)
(define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)

(define-key global-map (kbd "C-x C-d") 'dired)

;; Toggle features easily with: C-x t <key>
;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(defvar alf/toggle-map (make-sparse-keymap))
(define-key ctl-x-map "t" alf/toggle-map)
(define-key alf/toggle-map "c" 'column-number-mode)
(define-key alf/toggle-map "d" 'toggle-debug-on-error)
(define-key alf/toggle-map "f" 'auto-fill-mode)
(define-key alf/toggle-map "t" 'toggle-truncate-lines)
(define-key alf/toggle-map "w" 'toggle-word-wrap)
(define-key alf/toggle-map "q" 'toggle-debug-on-quit)
(define-key alf/toggle-map "g" 'god-mode)
(define-key alf/toggle-map "n" #'narrow-or-widen-dwim)
(define-key alf/toggle-map (kbd "RET") 'toggle-frame-fullscreen)

(require 'org)
;;; "C-x t n" to enter org src blocks, and "C-x C-s" to exit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))


(define-key ctl-x-map (kbd "C-z") 'magit-status)

;; I don't like the remap from prelude
(global-unset-key [remap other-window])
(define-key ctl-x-map "g" 'ace-window)

;;;
;;; create one that does.
(defun alf/key-chord-undefine (keys)
  "Undefine the key chord identified by KEYS.
This should be done by key-chord-unset-global, however that
does not work."
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (global-unset-key (vector 'key-chord key1 key2))
      ;; else
      (global-unset-key (vector 'key-chord key1 key2))
      (global-unset-key (vector 'key-chord key2 key1)))))

;; The jk key chord does not work well in Norwegian
(alf/key-chord-undefine "jk")
(key-chord-define-global "jc" 'ace-jump-char-mode)

;;; My very own prefix key
(defvar alf/ctl-z-map (make-sparse-keymap))
(define-key global-map (kbd "C-z") alf/ctl-z-map)
(define-key alf/ctl-z-map "z" 'ace-jump-mode)
(define-key alf/ctl-z-map "r" 'alf/recompile)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
