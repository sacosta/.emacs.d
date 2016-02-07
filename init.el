(require 'package)

;;Repos
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packagess/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


(defvar my-packages '(evil
		      ecb
		      key-chord
		      haskell-mode
                      swiper
                      counsel
                      paredit
                      idle-highlight-mode
                      ido-ubiquitous
                      find-file-in-project
                      magit
                      smex
                      scpaste))


(package-initialize)

;;Install missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;Fix haskell identation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;I want vi modes
(require 'evil)
  (evil-mode 1)

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)'

;;Activate ecb
(require 'ecb)

(setq
 backup-directory-alist '(("." . "~/.emacs_backups"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
 


;;Theme section
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'wombat t)
(set-face-attribute 'default nil :height 130)


