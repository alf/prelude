;;; setup-keybindings -- Global key bindings
;;; Commentary:

;; This package contains my global keybindings that are not mode
;; specific. That is, my global keybindings for helm, projectile and
;; org-mode are defined in their respective setup files.

;;; Code:

;; Switch easily between frames
(global-set-key (kbd "M-`") 'other-frame)

;; Toggle features easily with: C-x t <key>
;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'alf/toggle-map)
(define-key ctl-x-map "t" 'alf/toggle-map)
(define-key alf/toggle-map "c" 'column-number-mode)
(define-key alf/toggle-map "d" 'toggle-debug-on-error)
(define-key alf/toggle-map "e" 'toggle-debug-on-error)
(define-key alf/toggle-map "f" 'auto-fill-mode)
(define-key alf/toggle-map "l" 'toggle-truncate-lines)
(define-key alf/toggle-map "L" 'toggle-word-wrap)
(define-key alf/toggle-map "q" 'toggle-debug-on-quit)
(define-key alf/toggle-map "n" #'narrow-or-widen-dwim)
(define-key alf/toggle-map (kbd "RET") 'toggle-frame-fullscreen)

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
(define-prefix-command 'alf/ctl-z-map)
(define-key global-map (kbd "C-z") 'alf/ctl-z-map)
(define-key alf/ctl-z-map "z" 'ace-jump-mode)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here