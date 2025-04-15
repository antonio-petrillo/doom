;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrillo4@studenti.unina.it")

(setq doom-theme 'doom-one)

(setq default-input-method "italian-postfix")

(setq display-line-numbers-type 'relative)
(setq org-hide-emphasis-markers t)

(setq doom-font (font-spec :family "Aporetic Serif Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 36)
      doom-big-font (font-spec :family "Aporetic Serif Mono" :size 52))

(setq org-directory "~/Documents/Org/")
(setq denote-directory (expand-file-name "notes" "~/Documents/Org"))
(setq org-agenda-files `(,(expand-file-name "Agenda.org" org-directory)
                         ,(expand-file-name "Uni.org" org-directory)))

(add-hook 'org-mode-hook 'variable-pitch-mode)

(setq org-agenda-custom-commands
      `(("d" "Daily Agenda"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-deadline-warning-days 7)
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-overriding-header "Today 📆")))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting tasks 🕙")))))

        ("r" "Reading List"
         ((tags "reading"
                ((org-agenda-overriding-header "Currently reading 📖")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\\* Reading list .*"))))
          (tags "+book-reading"
                ((org-agenda-overriding-header "Next to read 📚")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\\* Reading list .*"))))))

        ("g" "Getting Things Done"
         ((tags "inbox"
                ((org-agenda-overriding-header "Inbox: 📬")))
          (alltodo "TODO"
                   ((org-agenda-overriding-header "Act: 📌")))
          (tags "explore" ;; refine
                ((org-agenda-overriding-header "Explore: 🔭")))))

        ("p" "Projects"
         ((tags "proj"
                ((org-agenda-overriding-header "Projects: 🛠️")
                 (org-agenda-skip-if 'done)))
          (tags "proj"
                ((org-agenda-overriding-header "Completed: ⚒️✅️")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))))))

        ("u" "Uni"
         ((tags "+uni-exam"
                ((org-agenda-overriding-header "Uni: 🎓")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\\* Uni .*"))))
          (tags "exam"
                ((org-agenda-overriding-header "Exams todo: 📄")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("PROGRESS")))))
          (tags "exam"
                ((org-agenda-overriding-header "Current courses: 🏢")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("TODO")))))
          (tags "exam"
                ((org-agenda-overriding-header "Remaining exams: 📆")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WAIT")))))
          (tags "+uni+proj"
                ((org-agenda-overriding-header "Projects: 💻")))
          (tags "exam done"
                ((org-agenda-overriding-header "Exams Completed: 📄✅")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))))))))

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
        "C-h" 'dired-hide-dotfiles-mode
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
    (cond (start? (backward-delete-char 1))
          (same? (backward-kill-word 1))
          (:else (kill-line 0)))))

(set-rotate-patterns! 'zig-mode
  :symbols '(("var" "const")))

(map!
 (:g "C-c a" #'org-agenda)

 (:leader :gnvi "n" nil)

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
   :desc "translate (it -> en)" "p" #'google-translate-at-point
   :desc "translate (en -> it)" "P" #'google-translate-at-point-reverse)

  (:prefix ("t" . "toggle")
   :desc "modeline" "M" #'hide-mode-line-mode)

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

;; HACK: for some reason this variable isn't defined when I load denote
(defvar denote-file-prompt-use-files-matching-regexp nil)

(use-package! denote
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (markdown-mode . denote-dired-mode))
  :config
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("emacs" "programming" "algorithm"
                                "datastructure" "cryptography" "logbook"
                                "film" "book" "meta" "exams"))
  :init
  (map! :leader
        (:prefix ("n" . "denote")
         :desc "create" "n" #'denote
         :desc "find" "f" #'(lambda () (interactive) (consult-find denote-directory))
         :desc "dired" "d" #'(lambda () (interactive) (dired denote-directory))
         :desc "menu" "m" #'denote-menu-list-notes
         :desc "rename" "r" #'denote-rename-file
         :desc "insert" "i" #'denote-link-or-create
         :desc "link" "l" #'denote-link-or-create
         :desc "select extension" "t" #'denote-type
         :desc "backlink" "b" #'denote-backlink

         (:prefix ("s" . "sequences")
          :desc "sequence" "n" #'denote-sequence
          :desc "dired" "f" #'denote-sequence-dired
          :desc "link" "i" #'denote-sequence-link
          :desc "link" "l" #'denote-sequence-link
          :desc "new child" "c" #'denote-sequence-new-child-of-current
          :desc "reparent" "r" #'denote-sequence-reparent
          :desc "new sibgling" "s" #'denote-sequence-new-sibling-of-current))))

;; WAIT: just to slow for now
;; (use-package! consult-denote
;;   :config
;;   (consult-denote-mode 1)
;;   :init
;;   (map! :leader
;;         (:prefix ("n" . "notes")
;;          :desc "consult find" "f" #'consult-denote-find
;;          :desc "consult grep" "g" #'consult-denote-grep)))

(setq! +evil-want-o/O-to-continue-comments nil
       evil-disable-insert-state-bindings t
       evil-kill-on-visual-paste nil)

(use-package! org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :config
  (setopt org-modern-table nil
          org-modern-star nil
          org-modern-block-fringe nil))

(use-package! org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :config
  (setopt org-startup-with-latex-preview t)
  (plist-put org-format-latex-options :scale 4)
  (plist-put org-format-latex-options :foreground 'auto)
  (plist-put org-format-latex-options :background 'auto))

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
    ";b" (nto/aas-expand-and-move "**** " 3)
    ";/" (nto/aas-expand-and-move "** " 2))
  (aas-set-snippets 'org-mode
    "mbb" (nto/aas-expand-and-move "\\mathbb{}" 1)
    "mca" (nto/aas-expand-and-move "\\mathcal{}" 1)
    ";ra" "\\rightarrow "
    ";la" "\\leftarrow "
    "__" (nto/aas-expand-and-move "_{}" 1)
    "^^" (nto/aas-expand-and-move "^{}" 1)
    "_sum" (nto/aas-expand-and-move "\\sum_{}" 1)
    "^sum" (nto/aas-expand-and-move "\\sum_{}^{}" 4)
    "_int" (nto/aas-expand-and-move "\\int_{}" 1)
    "^int" (nto/aas-expand-and-move "\\int_{}^{}" 4)
    ";b" (nto/aas-expand-and-move "** " 2)
    ";/" (nto/aas-expand-and-move "// " 2)
    ";A" "\\forall"
    ";E" "\\exists"
    ";|" "\\lor"
    ";&" "\\land"
    ";a" "\\alpha"
    ";;b" "\\beta"
    ";c" "\\gamma"
    ";d" "\\delta"
    ";e" "\\eta"
    ";E" "\\Eta"
    ";m" "\\mu"
    ";n" "\\nu"
    ";f" "\\phi"
    ";;f" "\\varphi"
    ";g" "\\nabla"
    ";s" "\\sigma"
    ";S" "\\Sigma"
    ";x" "\\times"
    ";." "\\cdot"
    ";;." "\\cdots"
    ";;$" (nto/aas-expand-and-move "$$$$ " 3)
    ";;4" (nto/aas-expand-and-move "$$$$ " 3)
    ";$" (nto/aas-expand-and-move "$$ " 2)
    ";4" (nto/aas-expand-and-move "$$ " 2)
    ";On" "O(n)"
    ";Oa" "O(1)"))

(use-package! trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; TODO: explore `tab-bar-format' variable:
;; try to add to the tab bar
;; - clock time
;; - battery display
