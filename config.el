;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'gruber-darker)

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrillo4@studenti.unina.it")

(setq display-line-numbers-type 'relative)

(setq org-directory "~/Documents/Org/"
      org-hide-emphasis-markers nil)

(setq doom-font (font-spec :family "JetBrains Mono" :style "Regular" :size 18)
      doom-big-font (font-spec :family "JetBrains Mono" :size 32)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 20))

(after! dired
  (use-package! dired-hide-dotfiles
    :custom (dired-listing-switches "-agho --group-directories-first")
    :config (evil-collection-define-key 'normal 'dired-mode-map
              "H" 'dired-hide-dotfiles-mode))

  (add-hook! 'dired-mode-hook #'dired-hide-dotfiles-mode)

  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    (kbd "m") 'dired-mark
    (kbd "t") 'dired-toggle-marks
    (kbd "u") 'dired-unmark
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "J") 'dired-goto-file
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
    (kbd "R") 'dired-do-rename
    (kbd "T") 'dired-do-touch
    (kbd "Y") 'dired-copy-filename-as-kill
    (kbd "+") 'dired-create-directory
    (kbd "-") 'dired-up-directory
    (kbd "% l") 'dired-downcase
    (kbd "% u") 'dired-upcase
    (kbd "; d") 'epa-dired-do-decrypt
    (kbd "; e") 'epa-dired-do-encrypt)

  (setq delete-by-moving-to-trash t))

(after! which-key

  (setq which-key-idle-delay 0.5)
  (setq which-key-allow-multiple-replacements t)

  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(setq doom-leader-alt-key "C-SPC")

(map! :leader
      :desc "M-x but faster" :n "SPC" #'execute-extended-command
      (:prefix ("f" . "file")
       :desc "Grep file" "g" #'consult-ripgrep
       :desc "Find file" "f" #'consult-find)
      (:prefix ("j" . "jump")
       :desc "jump to char" "j" #'avy-goto-char-timer
       :desc "jump to char (single)" "c" #'avy-goto-char
       :desc "jump to char 2" "J" #'avy-goto-char-2
       :desc "jump to word" "w" #'avy-goto-word-0
       :desc "jump to line" "l" #'avy-goto-line))

(after! evil
  (evil-define-key 'normal 'global
    (kbd "M-a")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-A")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-a")   #'evil-multiedit-match-and-next
    (kbd "M-A")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-d") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-a")   #'evil-multiedit-match-and-next
      (kbd "M-S-a") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev))

  ;; evil-mc
  (evil-define-key '(normal visual) 'global
    "gzm" #'evil-mc-make-all-cursors
    "gzu" #'evil-mc-undo-all-cursors
    "gzz" #'+evil/mc-toggle-cursors
    "gzc" #'+evil/mc-make-cursor-here
    "gzn" #'evil-mc-make-and-goto-next-cursor
    "gzp" #'evil-mc-make-and-goto-prev-cursor
    "gzN" #'evil-mc-make-and-goto-last-cursor
    "gzP" #'evil-mc-make-and-goto-first-cursor)

  (with-eval-after-load 'evil-mc
    (evil-define-key '(normal visual) evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
      (kbd "C-N") #'evil-mc-make-and-goto-last-cursor
      (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
      (kbd "C-P") #'evil-mc-make-and-goto-first-cursor))

  (evil-define-key '(insert normal) 'vterm-mode-map
    (kbd "C-k") #'vterm-send-up
    (kbd "C-j") #'vterm-send-down)

  (evil-define-key '(insert normal) 'global
    (kbd "C-d") #'delete-char
    (kbd "M-d") #'kill-word
    (kbd "C-n") #'next-line
    (kbd "C-p") #'previous-line))

(global-set-key (kbd "M-y") 'consult-yank-pop)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main")

  (when (eq system-type 'darwin)
    (setenv "PATH"
            (concat "/Library/TeX/texbin" ":"
		    (getenv "PATH"))))

  (when (eq system-type 'windows-nt)
    (setq default-directory "C:/Users/antof/"))

  (use-package! selectric-mode
    :commands selectric-mode)

  (use-package! google-translate
    :init
    (setq google-translate-translation-directions-alist
          '(("it" . "en") ("en" . "it")))
    (setq google-translate-default-source-language "it")
    (setq google-translate-default-target-language "en")
    (map! :leader
          (:prefix ("l" . "language")
           :desc "translate" "t" #'google-translate-smooth-translate
           :desc "translate (choice lang)" "T" #'google-translate-query-translate
           :desc "translate at point" "p" #'google-translate-at-point
           :desc "translate at point" "P" #'google-translate-at-point-reverse))
    (set-popup-rule! "*Google Translate*" :side 'bottom :ttl 3 :select t)) (setq doom-modeline-persp-name t))

(use-package! rotate
  :init
  (setq rotate-functions '(rotate:even-horizontal
                           rotate:even-vertical
                           rotate:main-horizontal
                           rotate:main-vertical
                           rotate:tiled)))

(map! :leader
      (:prefix ("r" . "rotate")
       :desc "rotate window" "r" #'rotate-window
       :desc "rotate window" "w" #'rotate-window
       :desc "rotate layout" "l" #'rotate-layout)
      (:prefix ("w" . "window")
       :desc "ace window" "w" #'ace-window))

(defun nto/org-roam-node-has-any-tags-p (node tags)
  "Predicate that return `t' if node has at least one of `tags', `nil' otherwise"
  (seq-intersection (org-roam-node-tags node) tags))

(defun nto/org-roam-node-filter-by-tags-any ()
  "Find and open an Org-roam node if it has any of the specified tags."
  (interactive)
  (let ((tags (completing-read-multiple "select tags: " (org-roam-tag-completions))))
    (org-roam-node-find nil nil (lambda (node) (nto/org-roam-node-has-any-tags-p node tags)))))

(defun nto/org-roam-node-has-all-tags-p (node tags)
  "Predicate that return `t' if node has all the `tags', `nil' otherwise"
  (not (seq-difference tags (org-roam-node-tags node))))

(defun nto/org-roam-node-filter-by-tags-all ()
  "Find and open an Org-roam node if it has all the specified tags."
  (interactive)
  (let ((tags (sort (completing-read-multiple "select tags: " (org-roam-tag-completions)) #'string-lessp)))
    (org-roam-node-find nil nil (lambda (node) (nto/org-roam-node-has-all-tags-p node tags)))))

(use-package! org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-db-location (expand-file-name "db/org-roam.db" org-directory))
  (org-roam-dailies-directory "daily/")
  :init
  (setq org-roam-completion-everywhere nil)
  (map! :leader
        (:prefix ("d" . "notes")
         :desc "node" "f" #'org-roam-node-find
         :desc "buffer" "b" #'org-roam-buffer-toggle
         :desc "gen ID" "I" #'org-id-get-create
         :desc "insert" "i" #'org-roam-node-insert
         (:prefix ("F" . "filter")
          :desc "all tags" "A" #'nto/org-roam-node-filter-by-tags-all
          :desc "any tags" "a" #'nto/org-roam-node-filter-by-tags-any)
         (:prefix ("t" . "tags")
          :desc "add" "a" #'org-roam-tag-add
          :desc "remove" "r" #'org-roam-tag-remove)
         (:prefix ("a" . "alias")
          :desc "add" "a" #'org-roam-alias-add
          :desc "remove" "r" #'org-roam-alias-remove)
         (:prefix ("r" . "refs")
          :desc "add" "a" #'org-roam-ref-add
          :desc "remove" "r" #'org-roam-ref-remove)
         (:prefix ("D" . "date")
          :desc "today" "t" #'org-roam-dailies-goto-today
          :desc "tomorrow" "T" #'org-roam-dailies-goto-tomorrow
          :desc "yesterday" "y" #'org-roam-dailies-goto-yesterday
          :desc "previous" "p" #'org-roam-dailies-goto-previous-note
          :desc "next" "n" #'org-roam-dailies-goto-next-note
          :desc "calendar" "c" #'org-roam-dailies-goto-date
          :desc "dired"    "d" #'org-roam-dailies-find-directory))))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        ort-roam-ui-open-on-start nil))

(use-package! consult-org-roam
  :after org-roam
  :diminish consult-org-roam-mode
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  (map! :leader
        (:prefix ("d" . "notes")
                 (:prefix ("c" . "consult")
                  :desc "find" "f" #'consult-org-roam-file-find
                  :desc "backlinks" "b" #'consult-org-roam-backlinks
                  :desc "forward links" "l" #'consult-org-roam-forward-links
                  :desc "search" "s" #'consult-org-roam-search)))
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-after-buffers t))

;; I use deft for quick notes that I made during lesson
;; Each deft will be inserted later inside org roam
(setq deft-directory (expand-file-name "deft" org-directory)
      deft-extensions '("org" "txt" "tex" "md")
      deft-recursive t)
(map! :leader
      (:prefix ("d" . "notes")
               (:prefix ("T" . "temp")
                :desc "deft ui" "f" #'deft
                :desc "new" "n" #'deft-new-file-named
                :desc "search" "s" #'deft-find-file
                :desc "delete" "d" #'deft-delete-file)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun nto/org-toggle-emphasis-markers ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers)))


;; (org-roam-db-location (expand-file-name "db/org-roam.db" org-directory))

(setq inbox (expand-file-name "agenda/inbox.org" org-directory)     ;; gtd inbox
      someday (expand-file-name "agenda/someday.org" org-directory) ;; store info about potential task, todo and projects
      backlog (expand-file-name "agenda/backlog.org" org-directory) ;; not properly gtd but I want to keep track of which `milestone' or `target' I've reached
      agenda (expand-file-name "agenda/agenda.org" org-directory) ;; act as calendar
      projects (expand-file-name "agenda/projects.org" org-directory)) ;; projects management

(setq org-agenda-files (list inbox someday agenda projects))

(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
      `(("i" "Inbox" entry  (file ,inbox)
	 ,(concat "* TODO %?\n"
		  "/Entered on/ %U"))
	("m" "meeting" entry  (file ,agenda)
	 ,(concat "* %? :meeting: \n"
		  "SCHEDULED <%<%Y-%m-%d %a %H:00>>"))
	("u" "urgent" entry  (file ,agenda)
	 ,(concat "* %? :urgent: \n"
		  "DEADLINE <%<%Y-%m-%d %a %H:00>>"))
	("o" "osint" entry  (file ,agenda)
	 ,(concat "* %? :osint: \n"
		  "<%<%Y-%m-%d %a %H:00>>"))))

(setq org-refile-targets
      `((,projects :maxlevel . 1)
	(,agenda :maxlevel . 1)
	(,someday :maxlevel . 1)
	(,backlog :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "ORGANIZE(o)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (todo "TODO"
                ((org-agenda-overriding-header "Todos to process")))
          (tags "project" ((org-agenda-overriding-header "Projects")))))
	("o" "Organize"
	 ((todo "TODO"
		((org-agenda-overriding-header "To organize")
		 (org-agenda-files org-agenda-files)))))
	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header)))))
	;; ("p" "Projects"
	;;  ((tags "project"
	;; 	((org-agenda-overriding-header "All Projects")))))
	("w" "Workflow Status"
	 ((todo "TODO"
		((org-agenda-overriding-header "Todos")
		 (org-agenda-files org-agenda-files)))
	  (todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next tasks")
		 (org-agenda-files org-agenda-files)))
	  (todo "DONE"
		((org-agenda-overriding-header "Completed")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "CANCELLED"
		((org-agenda-overriding-header "Cancelled")
		 (org-agenda-files org-agenda-files)))))))
