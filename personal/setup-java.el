;;; setup-java.el --- java-mode configuration.
;;
;; Copyright © 2014 Alf Lervåg
;;
;; Author: Alf Lervåg <alf@lervag.net>
;; URL: https://github.com/alf/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for java-mode

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

(prelude-require-package 'javadoc-lookup)
(require 'javadoc-lookup)
(setenv "JAVA_HOME" (string-trim
                     (shell-command-to-string  "/usr/libexec/java_home")))

(javadoc-add-artifacts [ch.qos.logback logback-classic "1.1.2"]
                       [ch.qos.logback logback-core "1.1.2"])



(add-hook 'java-mode-hook
          (lambda ()
            (ggtags-mode 1)))

(provide 'setup-java)

;;; setup-java.el ends here
