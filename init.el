(require 'package)

;;Repos
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(defvar my-packages '(evil
		      key-chord
		      haskell-mode
                      swiper
                      counsel
                      paredit
                      idle-highlight-mode
		      js2-mode
		      flycheck
                      ido-ubiquitous
                      find-file-in-project
		      org
		      ivy
		      rtags
		      swiper
                      magit
		      projectile
		      smex))
(package-initialize)

;;Install missing packages
(let ((packages-refresehd nil))
  (when (not package-archive-contents)
    (package-refresh-contents)
    (setq packages-refresehd t))

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (unless packages-refresehd
	(package-refresh-contents)
	(setq packages-refresehd t))
      (package-install p))))



(setq ns-right-alternate-modifier nil)

(setq js-indent-level 2)
(setq js2-strict-missing-semi-warning nil)

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


(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(setq
 backup-by-copying t
 backup-directory-alist `((".*" . "~/.emacs_backups"))
 auto-save-file-name-transforms `((".*", temporary-file-directory))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(add-hook 'js2-mode-hook
	  (lambda() (setq js2-basic-offset 2)))
				    

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (require 'company-rtags)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))
 


;;Theme section
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'gruvbox t)
(set-face-attribute 'default nil :height 150)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(package-selected-packages
   (quote
    (company company-shell company-rtags rtags cmake-ide cmake-mode rjsx-mode jsx-mode js2-mode ediprolog dante flymake-haskell-multi graphql-mode gruvbox-theme edts smex magit find-file-in-project ido-ubiquitous idle-highlight-mode paredit counsel swiper haskell-mode key-chord evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
