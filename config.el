;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrill4@studenti.unina.it")

;;(setq doom-font (font-spec :family "FiraMono" :size 12 :weight 'regular)
;;      doom-variable-pitch-font (font-spec :family "FiraSans" :size 13))

(setq doom-theme 'doom-old-hope)

(setq display-line-numbers-type 'relative
      org-hide-emphasis-markers nil)

(setq org-directory "~/Documents/Org/")

(use-package! ace-window
  :config
  (setq aw-scope 'frame)
  (setq aw-dispatch-always nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window "Flip window")
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?e aw-execute-command-other-window "Execute Command Other Window")
      (?c aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  :init
  (map! :leader
        (:prefix ("w" . "window")
         :desc "ace window" "w" #'ace-window)))

(after! dired
  (use-package! dired-hide-dotfiles
    :custom (dired-listing-switches "-agho --group-directories-first"))

  (map! :map dired-mode-map
        "H" #'dired-hide-dotfiles-mode
        "h" #'dired-up-directory
        "l" #'dired-find-file
        "m" #'dired-mark
        "t" #'dired-toggle-marks
        "u" #'dired-unmark
        "C" #'dired-do-copy
        "D" #'dired-do-delete
        "J" #'dired-goto-file
        "M" #'dired-do-chmod
        "O" #'dired-do-chown
        "R" #'dired-do-rename
        "T" #'dired-do-touch
        "Y" #'dired-copy-filename-as-kill
        "+" #'dired-create-directory
        "-" #'dired-up-directory
        "% l" #'dired-downcase
        "% u" #'dired-upcase
        "; d" #'epa-dired-do-decrypt
        "; e" #'epa-dired-do-encrypt)

  (add-hook! 'dired-mode-hook #'dired-hide-dotfiles-mode)

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
      (:prefix ("w" . "window")
       :desc "delete others" "1" #'delete-other-windows)
      (:prefix ("j" . "jump")
       :desc "jump to char" "j" #'avy-goto-char-timer
       :desc "consult line" "c" #'consult-line
       :desc "jump to char 2" "J" #'avy-goto-char-2
       :desc "jump to word" "w" #'avy-goto-word-0
       :desc "jump to line" "l" #'avy-goto-line
       :desc "jump to line below" "n" #'avy-goto-line-below
       :desc "jump to line above" "p" #'avy-goto-line-above
       :desc "jump to end line" "e" #'avy-goto-end-of-line)
      (:prefix ("l". "lang")
       :desc "translate (it -> en)" "p" #'google-translate-at-point
       :desc "translate (en -> it)" "P" #'google-translate-at-point-reverse))

(use-package! google-translate
  :config
  (set-popup-rule! "^\\*Google Translate" :slot -1 :size 0.2 :select t)
  (setq google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (setq google-translate-default-source-language "it")
  (setq google-translate-default-target-language "en"))

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

(use-package! denote
  :init
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-directory (expand-file-name "notes" org-directory))
  (setq denote-prompts '(title keywords))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("emacs" "programming" "algorithm" "datastructure"
                                "pattern" "math" "art" "music"
                                "film" "book" "philosophy" "meta"
                                "linux" "windows" "fitness"))
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-journal-extras-directory (expand-file-name "journal" denote-directory))

  (add-hook! 'dired-mode-hook #'denote-dired-mode)

  (map! :leader
        (:prefix ("n" . "notes")
         :desc "note find" "f" #'denote-open-or-create
         :desc "note dired" "d" #'list-denote
         :desc "note rename" "r" #'denote-rename-file
         :desc "note insert" "i" #'denote-link-or-create
         :desc "note link" "l" #'denote-link-or-create
         :desc "note backlink" "b" #'denote-backlink
         :desc "note journal" "j" #'denote-journal-extras-new-entry)))

(use-package! tab-bar
  :config
  (map! :leader
        (:prefix ("<tab>" . "workspaces")
         :desc "switch" "<tab>" #'tab-switch
         :desc "new" "n" #'tab-new
         :desc "buffer" "b" #'switch-to-buffer-other-tab
         :desc "dired" "d" #'dired-other-tab
         :desc "find file" "f" #'find-file-other-tab
         :desc "close" "c" #'tab-close
         :desc "rename" "r" #'tab-rename
         :desc "undo" "u" #'tab-undo))
  (global-set-key (kbd "C-<tab>") #'tab-next)
  (global-set-key (kbd "S-C-<tab>") #'tab-previous))

;; eglot conf
(after! eglot
  :config
  (set-eglot-client! 'odin-mode '("~/.local/src/ols/ols"))
  (set-eglot-client! 'gleam-mode '("gleam lsp")))

;; kill word hack
(defun nto/backward-kill-word()
  "Same as `backward-kill-word' but if it is invoked on a white space character
at the beginning of the line it will stop at it, furthermore if it is invoked
on the beginning of the line it will go the end of the previous line instead
of delete the previous word."
  (interactive)
  (let ((same? (save-excursion
                 (let ((orig (line-number-at-pos (point)))
                       (dest (progn
                               (backward-word)
                               (line-number-at-pos (point)))))
                   (eq orig dest))))
        (start? (eq (point) (line-beginning-position))))
    (cond (start? (backward-delete-char 1))
          (same? (backward-kill-word 1))
          (:else (kill-line 0)))))

(global-set-key (kbd "C-<backspace>") 'nto/backward-kill-word)
