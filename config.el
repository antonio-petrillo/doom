;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'anti-zenburn)

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrillo4@studenti.unina.it")

(setq display-line-numbers-type 'relative)

(setq org-directory "~/Documents/Org/"
      org-agenda-files `(
                         ,(concat org-directory "agenda/" "agenda.org")
                         ,(concat org-directory "agenda/" "scheduling.org")
                         ))

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
       :desc "jump to char" "j" #'avy-goto-char
       :desc "jump to char 2" "J" #'avy-goto-char-2
       :desc "jump to word" "w" #'avy-goto-word-0
       :desc "jump to line" "l" #'avy-goto-line))


(after! evil
  ;; evil-multiedit
  (evil-define-key 'normal 'global
    (kbd "M-a")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-A")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-a")   #'evil-multiedit-match-and-next
    (kbd "M-A")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-a") #'evil-multiedit-restore)

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

(use-package! denote)
(use-package! denote-menu)

(after! denote-menu

  (defun nto/journal-regex-today ()
    "Build a regex that match the entry journal of today."
    (format "%sT[0-9]\\{6\\}.*_%s"
            (format-time-string "%Y%m%d")
            denote-journal-extras-keyword))

  (defun nto/journal-entry-exist-p ()
    (car (directory-files denote-journal-extras-directory t (nto/journal-regex-today))))

  (defun nto/journal ()
    (interactive)
    (let ((today (nto/journal-entry-exist-p)))
      (if today
          (find-file today)
        (denote-journal-extras-new-entry))))

  (defun nto/dired-denote ()
    (interactive)
    (dired denote-directory))

  (require 'denote-org-dblock)
  (require 'denote-journal-extras)

  (setq denote-directory (expand-file-name "notes" org-directory))
  (setq denote-journal-extras-directory (expand-file-name "journal" org-directory))
  (setq denote-prompts '(title keywords))
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("journals" "tirocinio" "clojure" "java" "project"
                                "ideas" "emacs" "cli" "book" "health" "pattern" "engineering"
                                "architecture" "art" "math" "master" "philosophy"
                                "security" "music" "films" "series" "meta"))
  (setq denote-templates
        `((example . ,(concat "* heading 1"
                              "\n\n"
                              "* another heading 2"
                              "\n\n"))))

  (add-hook! 'dired-mode-hook #'denote-dired-mode)
  (add-hook! 'find-file-hook #'denote-link-buttonize-buffer)

  (map! :leader
        (:prefix ("d" . "denote")
         :desc "find" "f" #'denote
         :desc "subdir find" "s" #'denote-create-note-in-subdirectory
         :desc "search" "S" #'nto/dired-denote
         :desc "dired" "d" #'list-denotes
         :desc "journal" "j" #'nto/journal
         :desc "date" "D" #'denote-date
         :desc "rename" "R" #'denote-rename-file
         :desc "rename" "r" #'denote-rename-file-using-front-matter
         :desc "insert" "i" #'denote-link-or-create
         :desc "link" "l" #'denote-insert-link
         :desc "backlink" "b" #'denote-backlinks
         :desc "template" "t" #'denote-template-prompt
         :desc "query" "q" #'denote-org-dblock-insert-links))

  ;; FIXME: these shortcut doesn't work in denote-menu mode
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "g") #'denote-global-menu)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired))
