;;; setup-saltstack.el --- saltstack customizations
;;; Commentary:
;;;
;;; Customizations to make editing saltstack state files easier.

;;; Code:

(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

(provide 'setup-saltstack)
;;; setup-saltstack.el ends here
