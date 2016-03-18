;;; setup-python.el --- Emacs python configuration.
;;
;; Copyright © 2016 Alf Lervåg
;;
;; Author: Alf Lervåg <alf@lervag.net>
;; URL: https://github.com/alf/emacs.d
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

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
(setenv "PYTHONIOENCODING" "utf-8")
(setq python-shell-interpreter "python3")

(define-key inferior-python-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(provide 'setup-python)

;;; setup-python.el ends here
