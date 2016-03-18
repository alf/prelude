;;; setup-javascript.el --- javascript-mode configuration.
;;
;; Copyright © 2014 Alf Lervåg
;;
;; Author: Alf Lervåg <alf@lervag.net>
;; URL: https://github.com/alf/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for javascript-mode

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

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)

     ;; The code to record the class is identical to that for
     ;; Backbone so we just make an alias
     (defalias 'js2-imenu-record-react-class
       'js2-imenu-record-backbone-extend)

     (unless (loop for entry in js2-imenu-extension-styles
                   thereis (eq (plist-get entry :framework) 'react))
       (push '(:framework react
                          :call-re "\\_<React\\.createClass\\s-*("
                          :recorder js2-imenu-record-react-class)
             js2-imenu-extension-styles))

     (add-to-list 'js2-imenu-available-frameworks 'react)
     (add-to-list 'js2-imenu-enabled-frameworks 'react)))


(defun modify-syntax-table-for-jsx ()
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)

(add-hook 'js2-mode-hook 'js2-imenu-extras-setup)

(setq js-indent-level 2)
(setq jsx-indent-level 2)

(provide 'setup-javascript)
;;; setup-javascript.el ends here
