;;; borrowed-functions -- Convenient functions copied from the net
;;; Commentary:

;; This is where I gather all my general functions that are too small
;; and specific to be packaged properly but that I find indispensable.

;;; Code:


;; This is a really nice simplification for narrowing that i got from
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((org-in-src-block-p)
                (org-edit-src-code)
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(defun jira-org-linkify ()
  "Convert the symbol at point into an org link to the bouvet jira."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (issue (symbol-name (symbol-at-point)))
         (link (concat "https://jira.bouvet.no/browse/" issue)))
    (delete-region (car bounds) (cdr bounds))
    (org-insert-link nil link issue)))

;;; http://stackoverflow.com/questions/611831/how-to-url-decode-a-string-in-emacs-lisp
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(provide 'borrowed-functions)
;;; borrowed-functions.el ends here
