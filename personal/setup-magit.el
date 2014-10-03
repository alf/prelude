;;; setup-magit.el --- magit configuration
;;
;;; Commentary:

;; Tweak magit to make it better for me

;;; Code:

(unless (fboundp 'magit-gh-pulls-mode)
  (package-install 'magit-gh-pulls))

(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1))

(provide 'setup-magit)
;;; setup-magit.el ends here
