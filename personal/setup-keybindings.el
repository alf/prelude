;;; setup-keybindings -- Global key bindings
;;; Commentary:

;; This package contains my global keybindings.

;;; Code:
(require 'org)

;; Switch easily between frames
(global-set-key (kbd "M-`") 'other-frame)

(define-key global-map (kbd "C-x C-d") 'dired)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "C-x C-f") 'helm-quit-and-find-file)

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
(define-key alf/toggle-map "W" #'alf/toggle-delete-trailing-whitespace)
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
(defvar alf/ctl-z-map (make-sparse-keymap))
(define-key global-map (kbd "C-z") alf/ctl-z-map)
(define-key alf/ctl-z-map "r" 'alf/recompile)

(require 'avy)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-z w") 'avy-goto-word-1)
(global-set-key (kbd "C-z z") 'avy-goto-char-2)

(require 'multiple-cursors)
(global-set-key (kbd "C-z m") 'mc/mark-all-dwim)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
