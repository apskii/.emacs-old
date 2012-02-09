
;; general
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

(setq default-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq inhibit-splash-screen t)
(desktop-save-mode 1)

;; global-keybindings
(global-set-key (kbd "C-D") "δ")

;; highlight-parentheses
(add-to-list 'load-path "C:/Program Files (x86)/Emacs/site-lisp/")
(require 'highlight-parentheses)
(highlight-parentheses-mode 1)

;; color-theme
(add-to-list 'load-path "C:/Program Files (x86)/Emacs/site-lisp/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue2)

;; haskell
(load "C:/Program Files (x86)/Emacs/site-lisp/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)

;; octave
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; slime
(add-to-list 'load-path "C:/Program Files (x86)/Emacs/site-lisp/slime-2011-11-09/")  
(setq inferior-lisp-program "S:/prog/lang.lisp/sbcl/sbcl.exe")
(require 'slime)
(slime-setup)

;; geiser
(load-file "C:/Program Files (x86)/Emacs/site-lisp/geiser-0.1.3/elisp/geiser.el")
(setq geiser-impl-installed-implementations '(racket))