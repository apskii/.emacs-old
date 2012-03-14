;| Utils
(defmacro λ (&rest args)
  `(lambda ,(butlast args)
     ,(car (last args))))

(defun @ (f xs)
  (mapc f xs))

(defun map (f xs)
  (mapcar f xs))

(defun zip-with (f xs ys)
  (if (and xs ys)
      (cons (funcall f (car xs) (car ys))
	    (zip-with f (cdr xs) (cdr ys)))))

;| General
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(desktop-save-mode 1)
(setq inhibit-splash-screen t)

;| Global-Keybindings
(define-key global-map [(control \\)] [?\u03BB])
(define-key global-map (kbd "RET") 'newline-and-indent)

;| UTF-8
(setq default-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;| Paths [p]
(setq p:site-lisp "C:/Program Files (x86)/Emacs/site-lisp/"
      p:prog      "S:/prog/"
      p:sandbox   "S:/prog/sandbox/"
      p:hs-proj   "S:/prog/lang.haskell/-.proj/")

(@ (λ p (add-to-list 'load-path (concat p:site-lisp p)))
   '(""
     "color-theme-6.6.0"
     "slime-2011-11-09"))

;| Windows [w]
(setq w:code (car (get-buffer-window-list)))

;| Speedbar [w]
(require 'sr-speedbar)

(@ 'speedbar-add-supported-extension
   '(".hs" ".lhs" ".lisp"))

(sr-speedbar-toggle)

(setq w:sbar sr-speedbar-window)

(defun speedbar-set-directory (path)
  (select-window w:sbar)
  (setq default-directory path)
  (speedbar-update-contents))

;| Windows [w]
(setq w:repl (split-window w:sbar 15)
      w:menu (split-window w:sbar () t))

;| Navigation
(set-window-buffer w:menu "*scratch*")

(require 'button)

(with-current-buffer "*scratch*"
  (end-of-buffer)
  (macrolet
      (($ (s p)
	  `(progn
	     (insert-button
	      ,s 'action (λ _ (speedbar-set-directory ,p)))
	     (newline)
	     (newline))))
    ($ "hs-proj"   p:hs-proj)
    ($ "sandbox"   p:sandbox)
    ($ "prog"      p:prog)
    ($ "site-lisp" p:site-lisp)))

;| Color-Theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue2)

;| Haskell-Mode
(load (concat p:site-lisp
	      "haskell-mode-2.8.0/haskell-site-file.el"))

(@ (λ e (add-hook 'haskell-mode-hook e))
   '(turn-on-haskell-doc-mode
     turn-on-haskell-indentation
     font-lock-mode))

(require 'inf-haskell)

(inferior-haskell-start-process '("ghci"))
(set-window-buffer w:repl inferior-haskell-buffer)

;| Octave
(autoload 'octave-mode "octave-mod" nil t)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode)
	    auto-mode-alist))

(add-hook 'octave-mode-hook
          (λ (abbrev-mode 1)
	     (auto-fill-mode 1)
	     (if (eq window-system 'x)
		 (font-lock-mode 1))))

;| Slime
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation))

;| Geiser
(load-file (concat p:site-lisp "geiser-0.1.3/elisp/geiser.el"))
(setq geiser-impl-installed-implementations '(racket))