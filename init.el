(defgroup my ()
  "Customization group for my stuff.")

(defcustom my-site-lisp-path "C:/Program Files (x86)/Emacs/site-lisp/"
  "Path to my /Emacs/site-lisp/."
  :group 'my)

(defcustom my-tabs-are-spaces t
  "Whether I like to use spaces everywhere."
  :group 'my)

(defcustom my-code-is-utf-8 t
  "Whether I like to use utf-8 everywhere"
  :group 'my)

(defcustom my-code-directories-alist
  `((" [ haskell ] " . "S:/prog/lang.haskell/-.proj/")
    ("" . "")
    (" [ sandbox ] " . "S:/prog/sandbox/")
    ("  [ prog ]   " . "S:/prog/")
    ("" . "")
    ("[ site-lisp ]" . ,my-site-lisp-path)
    ("[ emacs-src ]" . "C:/Program Files (x86)/Emacs/emacs/lisp/"))
  "List of my most general directories with code."
  :group 'my)

(defvar my-code-window ()
  "Window for code.")

(defvar my-code-menu-window ()
  "Window for code-navigation menu with entries from `my-code-directories-alist'.")

(defvar my-interactive-window ()
  "Window for repls, completions, debuggers, etc.")

(defvar my-control-menu-window ()
  "Window for switching stuff in interactive window and other general stuff.")

;; Maximize emacs
(when (fboundp 'w32-send-sys-command)
  (w32-send-sys-command #xf030))

;; Utils
(defun site-load (path)
  (load (concat my-site-lisp-path path)))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

;; General
(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(menu-bar-mode     -1)
(blink-cursor-mode -1)

(setq blink-matching-paren  nil
      inhibit-splash-screen t)

;; Tabs
(when my-tabs-are-spaces
  (setq-default indent-tabs-mode nil))

;; Global-Keybindings
(define-key global-map [(control \\)] [?\u03BB]) ; λ
; (define-key global-map [(control \r)] [?\u03BC]) ; μ
; (define-key global-map [(control \v)] [?\u03BD]) ; ν
; (define-key global-map [(control \a)] [?\u2200]) ; ∀
; (define-key global-map [(control \e)] [?\u2203]) ; ∃
; (define-key global-map [(control \d)] [?\u0394]) ; Δ
(define-key global-map (kbd "RET") 'newline-and-indent)

;; UTF-8
(when my-code-is-utf-8
  (setq default-buffer-file-coding-system 'utf-8
        locale-coding-system              'utf-8)  

  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  
  (prefer-coding-system 'utf-8))

;; Font
(set-face-font 'default "-misc-cmu typewriter text-medium-r-normal--0-0-0-0-m-0-iso8859-1")

;; Auto-Load Site-Lisp Stuff
(mapc (lambda (path)
	(add-to-list 'load-path
		     (concat my-site-lisp-path
                             path)))
      '(""
	"color-theme-6.6.0"
	"slime-2011-11-09"
        "icicles"
        "doremi"
        "haskell-mode"
        "agda-mode"
        "php-mode-1.5.0"
        "js2-mode"))

;; Icicles
;(require 'icicles)
;(icy-mode 1)

;; Code-Window Init
(setq my-code-window (car (get-buffer-window-list)))

;; Speedbar
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

(defcustom my-speedbar-update-contents-hook nil
  "Hooks run when the update-contents executed."
  :group 'speedbar
  :type 'hook)

(defadvice speedbar-update-contents (after speedbar-update-contents-after-advice)
  (run-hooks 'my-speedbar-update-contents-hook))

(ad-activate 'speedbar-update-contents)

;; Windows
(setq my-interactive-window  (split-window sr-speedbar-window 15)
      my-code-menu-window    (split-window sr-speedbar-window () t)
      my-control-menu-window (split-window my-code-menu-window))

;; PowerShell
(autoload 'powershell "powershell"
  "Run powershell as a shell within emacs." t)

(setq powershell-process
      (get-buffer-process (powershell)))

(add-hook 'my-speedbar-update-contents-hook
          (lambda ()
            (process-send-string powershell-process
                                 (concat "cd \"" default-directory "\"\n"))))


; my-code-menu-window resizer (to 13 columns)
(push (lambda (_)
	(let ((w (window-width my-code-menu-window)))
	  (when (> w 13)
	    (with-selected-window my-code-menu-window
	      (shrink-window-horizontally (- w 13))))))
      window-size-change-functions)

; powershell-window resizer
(push (lambda (_)
	(if (= 59 (window-width my-interactive-window))
	    (powershell--set-window-width powershell-process 59)))
      window-size-change-functions)

; interactive-window resizer
(push (lambda (_)
	(let ((w (window-height my-interactive-window)))
	  (when (< w 38)
	    (with-selected-window my-interactive-window
	      (enlarge-window (- 38 w))))))
      window-size-change-functions)

; code-menu window resizer
(push (lambda (_)
	(let ((w (window-height my-code-menu-window)))
	  (when (< w 13)
	    (with-selected-window my-code-menu-window
	      (enlarge-window (- 13 w))))))
      window-size-change-functions)

;; Navigation
(require 'button)

; Menu-A
(defface green-link '((t (:foreground "green")))
  "Face for green links in upper navigation menu."
  :group 'basic-faces)

(let ((w my-code-menu-window))
  (set-window-buffer w (generate-new-buffer "menu-a"))
  (set-window-dedicated-p w t))

(with-current-buffer "menu-a"
  (end-of-buffer)
  (mapc (lambda (entry)
          (newline)
          (insert-button (car entry)
                         'action `(lambda (_)
                                    (speedbar-set-directory ,(cdr entry)))
                         'face 'green-link
                         'help-echo nil))
        my-code-directories-alist))

; Menu-B
(defface yellow-link '((t (:foreground "yellow")))
  "Face for yellow links in lower navigation menu."
  :group 'basic-faces)

(defface orange-link '((t (:foreground "orange")))
  "Face for orange links in lower navigation menu."
  :group 'basic-faces)

(defface red-link '((t (:foreground "red")))
  "Face for orange links in lower navigation menu."
  :group 'basic-faces)

(let ((w my-control-menu-window))
  (set-window-buffer w (generate-new-buffer "menu-b"))
  (set-window-dedicated-p w t))

(with-current-buffer "menu-b"
  (end-of-buffer)
  (flet (($ (label buffer &optional face action)
	    (newline)
	    (insert-button label
			   'action (or action
				       `(lambda (_)
					  (set-window-buffer my-interactive-window
							     ,buffer)))
			   'face (or face
				     'yellow-link)
			   'help-echo nil)))
    ($ "  [ ghci ]   " "*haskell*")
    ($ "   [ ps ]    " "*PowerShell*")
    (newline)
    ($ " [ buffers ] " () 'orange-link
       (lambda (_)
         (set-window-buffer my-interactive-window "*Buffer List*")
         (update-buffer-menu)))
    ($ "  [ trace ]  " "*Backtrace*" 'orange-link)
    (newline)
    ($ " [ init.el ] " ()
       'red-link
       (lambda (_)
	 (with-selected-window my-code-window
	   (set-window-buffer my-code-window
			      (or (get-buffer "init.el")
				  (find-file "~/.emacs.d/init.el"))))))))

;; Buffer-List Customization (Hacks!)
(setq my-buffers-list-buffer (window-buffer (list-buffers)))

(defun buffer-menu-mouse-select-other-window (event)
  "Select the buffer (in other window) whose line you click on."
  (interactive "e")
  (let (buffer)
    (with-current-buffer my-buffers-list-buffer
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (Buffer-menu-buffer t))))
    (with-selected-window my-interactive-window
      (switch-to-buffer-other-window buffer))))

(defun update-buffer-menu ()
  (with-selected-window my-interactive-window
    (with-current-buffer my-buffers-list-buffer
      (setq Buffer-menu-files-only t)
      (revert-buffer))))

(define-key Buffer-menu-mode-map [mouse-2]
  'buffer-menu-mouse-select-other-window)

(add-hook 'find-file-hook
          'update-buffer-menu)

;; Color-Theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue2)

;; Haskell-Mode
(site-load "haskell-mode/haskell-site-file.el")

(mapc (lambda (event)
	(add-hook 'haskell-mode-hook event))
      '(turn-on-haskell-doc-mode
	turn-on-haskell-indentation
	font-lock-mode))

(require 'inf-haskell)

(inferior-haskell-start-process '("ghci"))
(set-window-buffer my-interactive-window
		   inferior-haskell-buffer)

;; Agda-Mode
(autoload 'agda2-mode "agda2-mode"
  "Major mode for editing Agda files (version ? 2)." t)

(add-to-list 'auto-mode-alist '("\\.l?agda\\'" . agda2-mode))
(modify-coding-system-alist 'file "\\.l?agda\\'" 'utf-8)

(provide 'agda2)

;; Octave
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

;; Slime
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation))

;; Geiser
(site-load "geiser-0.1.3/elisp/geiser.el")
(setq geiser-impl-installed-implementations '(racket))

;; nxhtml
(site-load "nxhtml/autostart.el")

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; hamlet-mode
(require 'hamlet-mode)

;; DoReMi
(require 'doremi)
(require 'doremi-cmd)
(require 'doremi-frm)

;; Force some buffers to display in my-interactive-window
(setq special-display-function 'my-display-buffer
      special-display-regexps  '("^\\*.*"))

(defun my-display-buffer (buffer &rest _)  
  (set-window-buffer my-interactive-window buffer)
  my-interactive-window)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(agda2-highlight-face-groups (quote default-faces))
 '(agda2-include-dirs (quote ("S:/prog/agda/lib-0.6/src" ".")))
 '(js2-auto-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(speedbar-supported-extension-expressions (quote (".lisp" ".lhs" ".hs" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".agda"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
