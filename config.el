;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun nto/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'doom-after-init-hook #'nto/display-startup-time)

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrill4@studenti.unina.it")

;; (setq doom-font (font-spec :family "FiraMono" :size 12 :weight 'regular)
;;      doom-variable-pitch-font (font-spec :family "FiraSans" :size 13))

(setq doom-font (font-spec :family "Fira Code" :size 20 :weight 'regular))

(setq doom-theme 'doom-monokai-classic)

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

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "modeline" "M" #'hide-mode-line-mode))

(use-package! dired-hide-dotfiles
  :custom (dired-listing-switches "-agho --group-directories-first"))

(after! dired-hide-dotfiles

  (add-hook! 'dired-mode-hook #'dired-hide-dotfiles-mode)

  (map! :map dired-mode-map
        "C-H" #'dired-hide-dotfiles-mode
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

  (setq delete-by-moving-to-trash t))

(after! which-key

  (setq which-key-idle-delay 0.5)
  (setq which-key-allow-multiple-replacements t)

  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(setq doom-leader-alt-key "M-SPC")

(use-package! google-translate
  :config
  (set-popup-rule! "^\\*Google Translate" :slot -1 :size 0.2 :select t)
  (setq google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (setq google-translate-default-source-language "it")
  (setq google-translate-default-target-language "en"))

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
         :desc "note select extension" "t" #'denote-type
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
         :desc "undo" "u" #'tab-undo)

        (:prefix ("TAB" . "workspaces")
         :desc "switch" "TAB" #'tab-switch
         :desc "new" "n" #'tab-new
         :desc "buffer" "b" #'switch-to-buffer-other-tab
         :desc "dired" "d" #'dired-other-tab
         :desc "find file" "f" #'find-file-other-tab
         :desc "close" "c" #'tab-close
         :desc "rename" "r" #'tab-rename
         :desc "undo" "u" #'tab-undo))
  (map!
   :gnvi
   :desc "next-tab" "C-<tab>" #'tab-next
   :desc "previous-tab" "S-C-<tab>" #'tab-previous))

;; expand exec-path
;; odin
(add-to-list 'exec-path (format "%s/Code/Source/Odin" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/Code/Source/ols" (getenv "HOME")))
;; zig
(add-to-list 'exec-path (format "%s/Code/Source/Zig/zig-0.14" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/Code/Source/zls/zig-out/bin" (getenv "HOME")))
;; gleam
(add-to-list 'exec-path (format "%s/Code/Source/Gleam" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/.cache/rebar3/bin" (getenv "HOME")))
;; nim
(add-to-list 'exec-path (format "%s/.nimble/bin" (getenv "HOME")))
;; roc
(add-to-list 'exec-path (format "%s/Code/Source/roc" (getenv "HOME")))
;; go
(add-to-list 'exec-path (format "%s/go/bin" (getenv "HOME")))

;; lsp conf
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (add-to-list 'lsp-language-id-configuration '(gleam-mode . "gleam"))

  (setq-default lsp-auto-guess-root t)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (format "%s/Code/Source/ols/ols" (getenv "HOME")))
                    :activation-fn (lsp-activate-on "odin")
                    :server-id 'ols
                    :multi-root t))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "gleam lsp")
                    :activation-fn (lsp-activate-on "gleam")
                    :server-id 'gleam
                    :multi-root t)))

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

(map!
 (:g
  :desc "Consult yank" "M-y" #'consult-yank-pop
  :desc "Delete backward" "C-<backspace>" #'nto/backward-kill-word))

(evil-define-key 'normal 'global
  (kbd "M-a")   #'evil-multiedit-match-symbol-and-next
  (kbd "M-S-A")   #'evil-multiedit-match-symbol-and-prev)

(evil-define-key 'visual 'global
  (kbd "M-a")   #'evil-multiedit-match-and-next
  (kbd "M-S-A")   #'evil-multiedit-match-and-prev)

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

(evil-define-key '(insert normal) 'global
  (kbd "C-d") #'delete-char
  (kbd "M-d") #'kill-word
  (kbd "C-n") #'next-line
  (kbd "C-p") #'previous-line)

(setq! +evil-want-o/O-to-continue-comments nil)

;; roc - lang :: see https://gitlab.com/tad-lispy/roc-ts-mode
(use-package! roc-ts-mode
  :mode ("\\.roc\\'" . roc-ts-mode)
  :config
  (map! :map roc-ts-mode-map
        (:localleader
         "f" #'roc-ts-format
         "b" #'roc-ts-build
         "t" #'roc-ts-test
         "r" #'roc-ts-run
         "d" #'roc-ts-dev
         "c" #'roc-ts-check
         "e" #'roc-ts-repl
         (:prefix ("s" . "roc-start")
                  "a" #'roc-ts-start-app
                  "p" #'roc-ts-start-pkg
                  "u" #'roc-ts-start-update)))

  (set-popup-rule! (rx bol "*roc-ts-repl") :size 0.3)

  ;; Setup the LSP support:
  ;; For this to work, you'll need roc_language_server, which is distributed in
  ;; Roc releases, in your PATH.
  (when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
    (require 'lsp-mode)
    (add-to-list 'lsp-language-id-configuration '(roc-ts-mode . "roc"))
    (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_language_server")
                                          :activation-fn (lsp-activate-on "roc")
                                          :major-modes '(roc-ts-mode)
                                          :server-id 'roc_ls)))
  (when (modulep! :tools lsp +eglot)
    (set-eglot-client! 'roc-ts-mode '("roc_language_server")))
  (add-hook 'roc-ts-mode-local-vars-hook #'lsp!)

  ;; Formatting
  (set-formatter! 'roc-ts-format '("roc" "format" "--stdin" "--stdout") :modes '(roc-ts-mode))
  (setq-hook! 'roc-ts-mode-local-vars-hook
    +format-with 'roc-ts-format)

  ;; Keywords that should trigger electric indentation
  (set-electric! 'roc-ts-mode :words '("else"))

  ;; Extra ligatures
  (when (modulep! :ui ligatures +extra)
    (set-ligatures! 'roc-ts-mode
      :true "Bool.true" :false "Bool.false"
      :not "!"
      :and "&&" :or "||")
    (setq-hook! 'roc-ts-mode-hook
      prettify-symbols-compose-predicate #'+roc-ts-symbol-compose-p)
    (defun +roc-ts-symbol-compose-p (start end match)
      "Like `prettify-symbols-default-compose-p', except that if the
match is !, it checks that it's a logical NOT rather than the !
suffix operator (syntactic sugar for Task.await; see URL
  `https://www.roc-lang.org/tutorial#the-!-suffix')."
      (and (prettify-symbols-default-compose-p start end match)
           (or (not (equal match "!"))
               (and
                ;; character before isn't a word character
                (not (eq (char-syntax (char-before start))
                         ?w))
                ;; character after is a word character or open paren
                (memq (char-syntax (char-after end))
                      '(?\( ?w))))))))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))
