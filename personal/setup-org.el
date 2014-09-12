;;; setup-org.el --- org-mode configuration.

;;; Commentary:

;; My org-mode setup

;;; Code:

;; Standard keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Make sure my keymaps are available
(require 'setup-keybindings)
(define-key alf/ctl-z-map "p" 'alf/org-punch-dwim)
(define-key alf/ctl-z-map "j" 'org-clock-goto)
(define-key alf/ctl-z-map (kbd  "SPC") 'bh/clock-in-last-task)


(setq org-mobile-inbox-for-pull "~/Dropbox/Org/refile.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (file-name-as-directory
                     (expand-file-name "Org" dropbox-dir)))

(let ((default-directory org-directory))
  (setq org-agenda-files
        (mapcar 'expand-file-name
                (list "refile.org"
                      "todo.org"
                      "personal.org"
                      "journal.org"
                      "projects")))

  (setq org-default-notes-file (expand-file-name "refile.org"))

  (setq org-capture-templates
        `(("t" "todo" entry (file ,(expand-file-name "refile.org"))
           "* TODO %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("T" "todo under current clock" entry (clock)
           "* TODO %?\n	%i\n	%a" )
          ("c" "capture" entry (file ,(expand-file-name "refile.org"))
           "* %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("n" "note" entry (file ,(expand-file-name "refile.org"))
           "* %? :NOTE:\n	%a" :clock-in t :clock-resume t)
          ("i" "Interruptionion" entry (file ,(expand-file-name "refile.org"))
           "* %? :INTERRUPTION:\n	%i\n	%a" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org"))
           "* %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("b" "Blog idea" entry (file+headline ,(expand-file-name "blog-ideas.org") "Blog ideas")
           "** %?\n%u\n")
          ("r" "respond" entry (file ,(expand-file-name "refile.org"))
           "* TODO Respond to %:from on %:subject\n	SCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "PROJECT-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-HOLD-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil)))))

; Tags with fast selection keys
(setq org-tag-alist (quote (("PROJECT" . ?p)
                            ("PHONE" . ?p)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

;; Don't show done tasks in the agenda
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; Don't bother showing the past, the future is more interesting
;; We use reporting for the past
(setq org-agenda-start-on-weekday nil)

(defun alf/org-punch-dwim ()
  (interactive)
  (if (org-clock-is-active)
      (alf/org-punch-out)
    (alf/org-punch-in)))

(defun alf/org-punch-in ()
  (interactive)
  (org-mobile-pull)
  (org-clock-in '(4)))

(defun alf/org-punch-out ()
  (interactive)
  (org-clock-out)
  (save-some-buffers)
  (org-mobile-push))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))


(provide 'setup-org)
;;; setup-org.el ends here
