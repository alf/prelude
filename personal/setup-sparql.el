;;; setup-sparql.el --- org babel sparql settings
;;
;;; Commentary:

;; Some settings for using sparql mode and sparql in org babel.

;;; Code:

(prelude-require-package 'sparql-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sparql . t)))

(setq sparql-default-base-url "http://localhost:8890/sparql")

(setq org-babel-default-header-args:sparql
      '((:url . "http://localhost:8890/sparql")
        (:format . "text/csv")))

(defun alf/enable-socks ()
  (interactive)
  (setq socks-server '("Default server" "127.0.0.1" 8888 5))
  (setq url-gateway-method 'socks))

(defun alf/disable-socks ()
  (interactive)
  (setq socks-server nil)
  (setq url-gateway-method 'native))

(provide 'setup-sparql)
;;; setup-sparql.el ends here
