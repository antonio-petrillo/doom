;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Themes
(package! tron-legacy-theme)
(package! spacemacs-theme)
(package! gruber-darker-theme)
(package! anti-zenburn-theme)
(package! ef-themes)
(package! tomorrow-night-deepblue-theme)

(package! ace-window)
(package! google-translate)
(package! odin-mode
  :recipe (:host sourcehut
           :repo "mgmarlow/odin-mode"))
(package! gleam-ts-mode
  :recipe (:host github
           :repo "gleam-lang/gleam-mode"
           :branch "main"
           :files ("gleam-ts-*.el")))
(package! roc-ts-mode :pin "8a85436227a9fdc07bce9ad773a46ba78cb3cdd0")
(package! dired-hide-dotfiles)
(package! aas)

(package! denote)
(package! denote-silo)
(package! denote-org)
(package! denote-sequence)
(package! denote-explore)

(package! trashed)
(package! jinx)
(package! tempel)
(package! tempel-collection)
(package! auto-yasnippet :disable t)

;; TODO: remove when this is merged into main org-mode
(unpin! org)
(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
   (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
   :files
   (:defaults "etc")
   :build t :pre-build
   (with-temp-file "org-version.el"
     (require 'lisp-mnt)
     (let
         ((version
           (with-temp-buffer
             (insert-file-contents "lisp/org.el")
             (lm-header "version")))
          (git-version
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
              (buffer-string)))))
       (insert
        (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
        (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
        "(provide 'org-version)\n"))))
  :pin nil)

;; tmp
(package! lox-mode)
