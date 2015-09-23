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
(require 'subr-x)
(setenv "JAVA_HOME" (string-trim
                     (shell-command-to-string  "/usr/libexec/java_home")))


(require 'javadoc-lookup)
(setq javadoc-lookup-completing-read-function #'completing-read)

(javadoc-add-artifacts
 [org.slf4j slf4j-api "1.7.7"]
 [ch.qos.logback logback-classic "1.1.2"]
 [ch.qos.logback logback-core "1.1.2"]
 [nil javaee-api "7.0"] ;  Manually downloaded to cache dir
 [no.bouvet.sesam.common common-core "0.2-SNAPSHOT"]
 [no.bouvet.sesam.sdshare client-core "0.2.3-SNAPSHOT"]
 [no.bouvet.sesam.sdshare sdshare-client "0.2.3-SNAPSHOT"]
 [no.bouvet.sesam.sdshare sdshareserver-core "0.1-SNAPSHOT"]
 [no.bouvet.sesam.sdshare sdshare-server "0.1-SNAPSHOT"]
 [junit junit "4.11"]
 [io.dropwizard.metrics metrics-jvm "3.1.0"]
 [io.dropwizard.metrics metrics-core "3.1.0"]
 [io.dropwizard.metrics metrics-servlet "3.1.0"]
 [io.dropwizard.metrics metrics-servlets "3.1.0"])

(add-hook 'java-mode-hook
          (lambda ()
            (define-key java-mode-map (kbd "C-z a") 'add-java-import)
            (define-key java-mode-map (kbd "C-z d") 'javadoc-lookup)
            (ggtags-mode 1)))

(add-to-list 'sp-sexp-suffix '(java-mode syntax ""))
(require 'gud)

(defun alf/projectile-jdb-source-directories ()
  (let ((default-directory (projectile-project-root)))
    (projectile-files-via-ext-command "find . -type d -path '*/java' -print0")))

(defun alf/projectile-jdb (command-line)
  "Run jdb with COMMAND-LINE plus sourcepaths from the projectile project."
  (interactive
   (list (gud-query-cmdline 'jdb)))
  (let ((default-directory (projectile-project-root)))
    (jdb (concat command-line " -sourcepath" (s-join ":" (alf/projectile-jdb-source-directories))))))

(defun alf/mvn-continue ()
  "Run the compilation again from where it failed."
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (buffer-end 1))
    (search-backward-regexp "mvn <goals> \\(-rf .*\\)$")
    (let ((continue-from (match-string 1)))
      (compilation-start (concat (first compilation-arguments) " " continue-from)))))

(defun alf/recompile ()
  "Run the compilation again."
  (interactive)
  (with-current-buffer "*compilation*"
    (recompile)
    (sit-for 1)
    (goto-char (buffer-end 1))))

(defun alf/java-cleanup-imports ()
  "Simple helper for cleaning up the java imports."
  (interactive)
  (save-excursion
    (jdl/goto-last-import)
    (open-line 1)
    (let ((end (point-marker))
          (add-blank-before
           (lambda (s bound)
             (when (search-forward s bound t)
               (beginning-of-line)
               (open-line 1)))))
      (goto-char (buffer-end 0))
      (delete-matching-lines "^ *$" (point) end)
      (forward-line)
      (sort-lines nil (point) end)
      (funcall add-blank-before "import ch" end)
      (funcall add-blank-before "import com" end)
      (funcall add-blank-before "import java" end)
      (funcall add-blank-before "import no" end)
      (funcall add-blank-before "import org" end)
      (funcall add-blank-before "import static" end))))

(defun alf/debug(command)
  (interactive (list (gud-format-command "stop at %c:%l" nil)))
  (let ((default-directory (projectile-project-root)))
    (compilation-start "mvn -pl sdshare-client/core test -Dmaven.surefire.debug")

    (with-current-buffer "*compilation*"
      (while (progn
               (sit-for 1)
               (goto-char (buffer-end 1))
               (if (search-backward-regexp "Listening for transport dt_socket at address: \\([0-9]*\\)" nil t)
                   (let ((port (match-string 1)))
                     (alf/projectile-jdb (concat "jdb -attach localhost:" port))
                     (gud-basic-call command)
                     (gud-cont 0)
                     nil)
                 t)))
      )
    (with-current-buffer "*compilation*"
      (while (progn
               (sit-for 1)
               (goto-char (buffer-end 1))
               (if (search-backward-regexp "Compilation \\(failed\\|finished\\)" nil t)
                   (if (string-equal "failed" (match-string 1))
                       (and nil (error "failed"))
                     nil)
                 t))))))

(defadvice sort-java-imports (after java-cleanup-imports activate compile)
  "Fix the java imports after my liking when using javadoc-add-artifacts."
  (alf/java-cleanup-imports))

(defun alf/mvn-debug ()
  "Run the compilation again from where it failed."
  (interactive)
  (with-current-buffer "*compilation*"
    (compilation-start (concat (first compilation-arguments) " -Dmaven.surefire.debug=true"))))

(sp-local-pair 'java-mode "<" ">")

(provide 'setup-java)

;;; setup-java.el ends here
