;;; setup-editor.el --- editor customizations

;;
;; Copyright © 2014 Alf Lervåg
;;
;; Author: Alf Lervåg <alf@lervag.net>
;; URL: https://github.com/alf/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Refinements of the core editing experience in Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-packages)

(prelude-require-packages
 '(ggtags
   ag
   n3-mode
   helm-ag
   restclient
   avy
   dot-mode))

(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(require 'n3-mode)

(require 'helm)
(require 'grep)
(setq helm-split-window-default-side 'right)
(setq helm-split-window-in-side-p nil)

(setq enable-recursive-minibuffers t)

(require 'helm-projectile)
(helm-projectile-on)

;; Prefer eww when opening urls
(setq browse-url-browser-function 'eww-browse-url)

(defun alf/remove-colors ()
  "Remove all colors in region or whole buffer.
Useful for getting rid of unsightly colors in eww"
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (mark-whole-buffer))
    (add-face-text-property (region-beginning)
                            (region-end)
                            (list :background "#3f3f3f")
                            nil)))

(require 'guru-mode)
(diminish 'smartparens-mode "🄢")
(diminish 'company-mode "✓")
(diminish 'guru-mode)
(diminish 'helm-mode "⎈")
(setq projectile-mode-line '(:eval
                             (format "℞[%s]"
                                    (projectile-project-name))))
(diminish 'flycheck-mode "✈")
(diminish 'abbrev-mode)
(diminish 'prelude-mode)

(setq prelude-guru nil)
(setq prelude-whitespace nil)

(setq-default alf/do-delete-trailing-whitespace t)
(defun alf/delete-trailing-whitespace ()
  (interactive)
  (when alf/do-delete-trailing-whitespace
    (delete-trailing-whitespace)))

(defun alf/toggle-delete-trailing-whitespace ()
  (interactive)
  (setq alf/do-delete-trailing-whitespace (not alf/do-delete-trailing-whitespace)))

(add-hook 'before-save-hook 'alf/delete-trailing-whitespace)

(defun alf/org-open-at-point-global (&optional use-eww)
  "Let me choose to use eww or the os browser."
  (interactive "P")
  (let ((browse-url-browser-function
         (if (equal '(4) use-eww) browse-url-browser-function
           'browse-url-default-browser)))
    (org-run-like-in-org-mode 'org-open-at-point)))

(global-set-key (kbd "C-c O") 'alf/org-open-at-point-global)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(server-start)
(require 'magit)

;; Enable tramp sudo proxy over ssh
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(provide 'setup-editor)
;;; setup-editor.el ends here
