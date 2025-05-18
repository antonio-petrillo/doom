;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrillo4@studenti.unina.it")

(setq doom-theme 'doom-one)

(setq default-input-method "italian-postfix")

(setq display-line-numbers-type 'relative)
(setq org-hide-emphasis-markers t)

(setq doom-font (font-spec :family "Aporetic Serif Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 20)
      doom-big-font (font-spec :family "Aporetic Serif Mono" :size 36))

(setopt xref-search-program 'ripgrep)

(setq-default evil-escape-key-sequence "jk")


(after! corfu
  (setopt corfu-preselect 'first
          corfu-quit-at-boundary t)
  (map! :map corfu-map
        "C-SPC" #'corfu-insert-separator))

(setq org-directory "~/Documents/Org/")
(setq denote-directory (expand-file-name "notes" "~/Documents/Org"))
(setq tempel-path (expand-file-name "templates/*.eld" doom-user-dir))

(setq org-agenda-files `(,(expand-file-name "Agenda.org" org-directory)
                         ;; add more here
                         ))

(after! org

  (setq org-agenda-custom-commands
        `(("u" "Uni"
           ((todo "EXAM"
                  ((org-agenda-overriding-header "Exams Todo: ")))
            (todo "CURRENT"
                  ((org-agenda-overriding-header "Current Courses: ")))
            (todo "QUEUED"
                  ((org-agenda-overriding-header "Next Courses: ")))
            (todo "PASSED"
                  ((org-agenda-overriding-header "Passed: "))))
           ((org-agenda-files '(,(expand-file-name "Uni.org" org-directory)))))
          ("n" "Notes"
           ((todo "TODO"
                  ((org-agenda-overriding-header "Notes to complete: "))))
           ((org-agenda-files '(,denote-directory))))))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "SKIP(s)"
           "WAIT(w)"  ; Something external is holding up this task
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable

          (sequence
           "EXAM(e)"
           "CURRENT(c)"
           "QUEUED(q)"
           "|"
           "PASSED(p)"))

        org-todo-keyword-faces
        '(("EXAM"  . +org-todo-todo)

          ("CURRENT" . +org-todo-active)

          ("QUEUED"  . +org-todo-onhold)
          ("SKIP"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)

          ("KILL" . +org-todo-cancel))))

(map! :leader :gnvi "p" nil)
(map! :leader
      (:prefix ("p" . "project")
       :desc "switch buffer" "b" #'project-switch-to-buffer
       :desc "switch project" "p" #'project-switch-project
       :desc "compile" "c" #'project-compile
       :desc "find file" "f" #'project-find-file
       :desc "kill buffer" "k" #'project-kill-buffers
       :desc "async command" "&" #'project-async-shell-command
       :desc "eshell" "e" #'project-eshell))
(define-key ctl-x-map "p" project-prefix-map)

(use-package! denote
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (markdown-mode . denote-dired-mode))
  :config
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords template))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("emacs" "programming" "algorithm" "meta"))
  (setq denote-templates
        '((empty . "")
          (meta . "Meta note that talks about ")
          (todo . "* TODO this note will talk about "))))

(with-eval-after-load 'org-capture
  (setopt org-capture-templates
          '(("n" "new note" plain
             (file denote-last-path)
             #'denote-org-capture
             :no-save t
             :immediate-finish nil
             :kill-buffer t
             :jump-to-captured t))))

(map! :leader :gnvi "n" nil)
(map! :leader
      (:prefix ("n" . "denote")
       :desc "new" "n" #'denote-open-or-create
       :desc "find" "f" #'(lambda () (interactive) (consult-find denote-directory))
       :desc "dired" "d" #'(lambda () (interactive) (dired denote-directory))
       :desc "insert" "i" #'denote-link-or-create
       :desc "link" "l" #'denote-link-or-create
       :desc "select extension" "t" #'denote-type
       :desc "grep" "g" #'denote-grep
       :desc "backlinks" "b" #'denote-backlinks
       :desc "rename using front matter" "r" #'denote-rename-file-using-front-matter
       :desc "rename using front matter (all file)" "C-r" #'denote-explore-sync-metadata

       (:prefix ("q" . "query")
        :desc "contents" "c" #'denote-query-contents-link
        :desc "filenames" "f" #'denote-query-filenames-link)

       (:prefix ("s" . "sequence")
        :desc "new" "n" #'denote-sequence
        :desc "link" "l" #'denote-sequence-link
        :desc "insert" "i" #'denote-sequence-link
        :desc "find" "f" #'denote-sequence-find
        :desc "new child" "c" #'denote-sequence-new-child-of-current
        :desc "new sibling" "s" #'denote-sequence-new-sibling-of-current
        :desc "reparent" "r" #'denote-sequence-reparent)

       (:prefix ("S" . "silos")
        :desc "dired" "d" #'denote-silo-dired
        :desc "new" "n" #'denote-silo-open-or-create
        :desc "change directory" "c" #'denote-silo-cd
        :desc "select and then cmd" "s" #'denote-silo-select-silo-then-command)))

(use-package! ace-window
  :config
  (setopt aw-scope 'frame)
  (setopt aw-dispatch-always nil)
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setopt aw-dispatch-alist
          '((?x aw-delete-window "Delete Windows")
            (?m aw-swap-window "Swap Windows")
            (?M aw-move-window "Move Windows")
            (?c aw-move-window "Move Windows")
            (?j aw-switch-buffer-in-window "Select Buffer")
            (?f aw-flip-window "Previous window")
            (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
            (?e aw-execute-command-other-window "Execute Command Other Window")
            (?S aw-split-window-fair "Split Fair Window")
            (?v aw-split-window-vert "Split Vert Window")
            (?b aw-split-window-horz "Split Horz Window")
            (?o delete-other-windows "Delete Other Windows")
            (?? aw-show-dispatch-help "Help"))))

(after! dired
  (use-package! dired-hide-dotfiles)
  (add-hook! 'dired-mode-hook  #'dired-hide-dotfiles-mode)
  (map! :map dired-mode-map
        "C-h" #'dired-hide-dotfiles-mode
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
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(use-package! jinx
  :hook ((org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (text-mode . jinx-mode))
  :config
  (setopt jinx-languages "en_US,it_IT"))

(use-package! google-translate
  :config
  (set-popup-rule! "^\\*Google Translate" :slot -1 :size 0.2 :select t)
  (setq google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (setq google-translate-default-source-language "it")
  (setq google-translate-default-target-language "en"))

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
    (cond (start? (delete-char -1))
          (same? (backward-kill-word 1))
          (:else (kill-line 0)))))

(set-rotate-patterns! 'zig-mode
  :symbols '(("var" "const")))

(after! evil
  (map! (:g "C-c a" #'org-agenda
            "C-," #'embark-dwim)))

(map!
 (:when (modulep! :editor snippets)
   ;; auto-yasnippet
   :i  [C-tab] nil
   :nv [C-tab] nil)

 (:v  "R"     #'evil-multiedit-match-all
  :n  "M-a"   #'evil-multiedit-match-symbol-and-next
  :n  "M-A"   #'evil-multiedit-match-symbol-and-prev
  :v  "M-a"   #'evil-multiedit-match-and-next
  :v  "M-A"   #'evil-multiedit-match-and-prev
  :nv "C-M-a" #'evil-multiedit-restore

  :n  "M-d"   nil
  :n  "M-D"   nil
  :v  "M-d"   nil
  :v  "M-D"   nil
  :nv "C-M-d" nil
  (:after evil-multiedit
          (:map evil-multiedit-mode-map
           :nv "M-a" #'evil-multiedit-match-and-next
           :nv "M-A" #'evil-multiedit-match-and-prev
           :nv "M-d" nil
           :nv "M-D" nil
           [return]  #'evil-multiedit-toggle-or-restrict-region)))
 (:gni
  :desc "next line" "C-n" #'next-line
  :desc "prev line" "C-p" #'previous-line
  :desc "delete char" "C-d" #'delete-char
  :desc "kill word" "M-d" #'kill-word)

 (:g
  :desc "Consult yank" "M-y" #'consult-yank-pop
  :desc "Delete backward" "C-<backspace>" #'nto/backward-kill-word)

 (:gnvi
  :desc "next tab" "C-<tab>" #'tab-next
  :desc "previous tab" "S-C-<tab>" #'tab-previous)

 (:leader
  :desc "Exec Cmd" :n "SPC" #'execute-extended-command
  :desc "Exec Cmd for buffer" :n "M-SPC" #'execute-extended-command-for-buffer

  (:prefix ("f" . "file")
   :desc "Grep file" "g" #'consult-ripgrep
   :desc "Find file" "f" #'consult-find)

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
   :desc "translate (it -> en)" "t" #'google-translate-at-point
   :desc "translate (en -> it)" "T" #'google-translate-at-point-reverse
   :desc "spellcheck" "c" #'jinx-correct ;; C-u SPC l c -> correct whole buffer
   :desc "languages" "l" #'jinx-languages
   :desc "next err" "n" #'jinx-next
   :desc "previous err" "p" #'jinx-previous)

  (:prefix ("s" . "search")
   :desc "browse url" "U" #'browse-url)

  (:prefix ("t" . "toggle")
   :desc "modeline" "m" #'hide-mode-line-mode)

  (:prefix ("e" . "execute")
   :desc "async shell command" "c" #'async-shell-command)

  (:prefix ("w" . "window")
   :desc "delete others" "1" #'delete-other-windows
   :desc "delete this" "0" #'delete-window
   :desc "other window" "o" #'other-window
   :desc "ace window" "w" #'ace-window
   :desc "maximize frame" "M" #'toggle-frame-maximized)

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
   :desc "undo" "u" #'tab-undo)))

(after! evil
  (setopt +evil-want-o/O-to-continue-comments nil
          evil-disable-insert-state-bindings t
          evil-move-cursor-back nil
          evil-kill-on-visual-paste nil))

(use-package! org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :config
  (setopt org-modern-table nil
          org-modern-star nil
          org-modern-block-fringe nil))

(use-package! org-latex-preview
  :config
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (setopt org-latex-preview-live t)
  (setopt org-latex-preview-live-debounce 0.25))

(use-package! trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S")
  :init
  (map! :leader
        :desc "Trash" "C-," #'trashed))

;; Configure Tempel
(use-package! tempel
  :hook ((conf-mode . nto/tempel-setup-capf)
         (prog-mode . nto/tempel-setup-capf)
         (text-mode . nto/tempel-setup-capf)
         )
  :custom
  (tempel-trigger-prefix ";")

  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))

  :init
  (defun nto/tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))

(map! :map tempel-map
      "RET" #'tempel-done
      "C-e" #'tempel-end
      "C-n" #'tempel-next
      [tab] #'tempel-next
      "TAB" #'tempel-next
      "C-p" #'tempel-previous
      [S-tab] #'tempel-previous
      "S-TAB" #'tempel-previous
      "C-q" #'tempel-abort
      "C-a" #'tempel-beginning)

(defmacro nto/aas-expand-and-move (snip offset)
  `(lambda () (interactive)
     (insert ,snip)
     (backward-char ,offset)))

(use-package! aas
  :hook
  ((org-mode . aas-activate-for-major-mode)
   (markdown-mode . aas-activate-for-major-mode)
   (latex-mode . aas-activate-for-major-mode))
  :config
  (aas-set-snippets 'markdown-mode
    ";b" '(tempel "**" p "** " q)
    ";/" '(tempel "*" p "* " q))
  (aas-set-snippets 'org-mode
    ";mb" '(tempel "\\mathbb{" p "} " q)
    ";mc" '(tempel "\\mathcal{" p "} " q)
    ";b" '(tempel "*" p "* " q)
    ";/" '(tempel "/" p "/ " q)
    ";-" '(tempel "_" p "_ " q)
    ";i" '(tempel "src_" p "{" q "}")
    ";>" "\\implies "
    ";<" "\\impliedby "
    ";'" "\\prime "
    ";." "\\cdot "
    ";;." "\\cdots "
    ";;4" '(tempel "$$" p "$$ " q)
    ";4" '(tempel "$$" p "$$ " q)))
