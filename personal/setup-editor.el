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

(prelude-require-package 'ggtags)
(prelude-require-package 'ag)
(prelude-require-package 'n3-mode)
(require 'n3-mode)

;;; Useful for getting rid of unsightly colors in eww
(defun alf/remove-colors ()
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (mark-whole-buffer))
    (add-face-text-property (region-beginning)
                            (region-end)
                            (list :background "#3f3f3f")
                            nil)))

  (provide 'setup-editor)

;;; setup-editor.el ends here
