;| Maximize emacs
(w32-send-sys-command #xf030)

;| Utils
(defmacro x2 (form)
  `(progn ,form
	  ,form))

(defun site-load (path)
  (load (concat site-lisp-path path)))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

;| General
(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(menu-bar-mode     -1)
(blink-cursor-mode -1)

(setq blink-matching-paren  nil
      inhibit-splash-screen t)

;| Global-Keybindings
(define-key global-map [(control \\)] [?\u03BB])
(define-key global-map (kbd "RET") 'newline-and-indent)

;| UTF-8
(setq default-buffer-file-coding-system 'utf-8
      locale-coding-system              'utf-8)

(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;| Paths
(setq site-lisp-path "C:/Program Files (x86)/Emacs/site-lisp/"
      prog-path      "S:/prog/"
      sandbox-path   "S:/prog/sandbox/"
      hs-proj-path   "S:/prog/lang.haskell/-.proj/")

(mapc (lambda (path)
	(add-to-list 'load-path
		     (concat site-lisp-path path)))
      '(""
	"color-theme-6.6.0"
	"slime-2011-11-09"))

;| Code-Window Init
(setq code-window (car (get-buffer-window-list)))

;| Speedbar
(require 'speedbar)
(require 'sr-speedbar)

(nconc speedbar-ignored-modes
       '(shell-mode
	 inferior-haskell-mode))

(mapc 'speedbar-add-supported-extension
      '(".hs" ".lhs" ".lisp"))

(sr-speedbar-toggle)

(defun speedbar-set-directory (path)
  (select-window sr-speedbar-window)
  (setq default-directory path)
  (speedbar-update-contents))

;| Windows
(setq interactive-window      (split-window sr-speedbar-window 15)
      speedbar-menu-window    (split-window sr-speedbar-window () t)
      interactive-menu-window (split-window speedbar-menu-window))

;| PowerShell
(autoload 'powershell "powershell"
  "Run powershell as a shell within emacs." t)

(setq powershell-process
      (get-buffer-process (powershell)))

(push (lambda ()
	(process-send-string powershell-process
			     (concat "cd \"" default-directory "\"\n")))
      speedbar-update-contents-hook)


; speedbar-menu-window resizer (to 13 columns)
(push (lambda (_)
	(let ((w (window-width speedbar-menu-window)))
	  (when (> w 13)
	    (with-selected-window speedbar-menu-window
	      (shrink-window-horizontally (- w 13))))))
      window-size-change-functions)

; powershell-window resizer
(push (lambda (_)
	(if (= 59 (window-width interactive-window))
	    (powershell--set-window-width powershell-process 59)))
      window-size-change-functions)

;| Navigation
(require 'button)

(set-window-buffer speedbar-menu-window
		   (generate-new-buffer "menu-a"))

(defface green-link '((t (:foreground "green")))
  "Face for green links in upper navigation menu."
  :group 'basic-faces)

(with-current-buffer "menu-a"
  (end-of-buffer)
  (flet (($ (label path)
	    (x2 (newline))
	    (insert-button label
			   'action `(lambda (_)
				      (speedbar-set-directory ,path))
			   'face 'green-link
			   'help-echo nil)))
    ($ " [ hs-proj ] "  hs-proj-path)
    ($ " [ sandbox ] "  sandbox-path)
    ($ "  [ prog ]   "  prog-path)
    ($ "[ site-lisp ]"  site-lisp-path)))

(set-window-buffer interactive-menu-window
		   (generate-new-buffer "menu-b"))

(defface yellow-link '((t (:foreground "yellow")))
  "Face for yellow links in lower navigation menu."
  :group 'basic-faces)

(defface orange-link '((t (:foreground "orange")))
  "Face for orange links in lower navigation menu."
  :group 'basic-faces)

(with-current-buffer "menu-b"
  (end-of-buffer)
  (flet (($ (label buffer &optional face action)
	    (x2 (newline))
	    (insert-button label
			   'action (or action
				       `(lambda (_)
					  (set-window-buffer interactive-window
							     ,buffer)))
			   'face (or face
				     'yellow-link)
			   'help-echo nil)))
    ($ "  [ ghci ]   " "*haskell*")
    ($ "   [ ps ]    " "*PowerShell*")
    (newline)
    ($ " [ init.el ] " ()
       'orange-link
       (lambda (_)
	 (with-selected-window code-window
	   (set-window-buffer code-window
			      (or (get-buffer "init.el")
				  (find-file "~/.emacs.d/init.el"))))))))

;| Color-Theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue2)

;| Haskell-Mode
(load "S:/prog/lang.elisp/haskell-mode/haskell-site-file.el")

(mapc (lambda (event)
	(add-hook 'haskell-mode-hook event))
      '(turn-on-haskell-doc-mode
	turn-on-haskell-indentation
	font-lock-mode))

(require 'inf-haskell)

(inferior-haskell-start-process '("ghci"))
(set-window-buffer interactive-window
		   inferior-haskell-buffer)

;| Octave
(autoload 'octave-mode "octave-mod" nil t)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode)
	    auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

;| Slime
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation))

;| Geiser
(site-load "geiser-0.1.3/elisp/geiser.el")
(setq geiser-impl-installed-implementations '(racket))
