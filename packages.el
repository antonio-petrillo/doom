;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! tron-legacy-theme)
(package! spacemacs-theme)
(package! gruber-darker-theme)
(package! anti-zenburn-theme)
(package! ef-themes)

(package! dired-hide-dotfiles)
(package! google-translate)
(package! denote)
(package! ace-window)

(package! odin-mode
  :recipe (:host sourcehut
           :repo "mgmarlow/odin-mode"))

(package! gleam-mode
  :recipe (:host github
           :repo "gleam-lang/gleam-mode"))

(package! roc-ts-mode)
