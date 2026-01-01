;;; Init.el ---  -*- lexical-binding: t; -*-

;;; Commentary: F10 opens the menu bar. Useful keyboard shortcuts at https://github.com/VernonGrant/emacs-keyboard-shortcuts. Note C-x <left> and C-x <right> will produce previous and next buffers, respectively.
;; Note M-x bug-hunter will bisect file for debugging; M-x bug-hunter-init-file RET a (featurep 'cl) RET


;;; Code:

;;; Use Emacs Doom
;; emacs --init-dir ~/emacs-distros/doom

;;(require 'package)
;; At some point, we may deploy straight.el and early-init.el

;; (setq package-archives
;;       '(("melpa" . "https://melpa.org/packages/")
;;         ("elpa" . "https://elpa.gnu.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;         ))
;; (add-to-list 'package-archives
;; 	     '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)

;; ;;; Initialize packages
;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;;; Use-package to install and configure packages
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'use-package)
;; ;; Eliminate need to "ensure t" in loading packages
;; (setq use-package-always-ensure t)
;; (setq load-prefer-newer t)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setting to avoid repeating :ensure t for each package
(setq use-package-always-ensure t)

;; Removes unneeded text on mode line
(require 'diminish)
;; Simplify custom key definitions
(require 'bind-key)


;;; Display line numbers when in program or configuration mode
(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

;; Start in note-taking directory
(setq default-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/")


;;; Common lisp library for macros only
;; (eval-when-compile (require 'cl-lib))
;;; Common lisp library
;; (require 'cl-lib)

;;; Available languages in org mode
;; (org-babel-do-load-languages 'org-babel-load-languages '((python. t) (ditaa .t) (R . t) (perl . t)))


;;; Load themes see https://protesilaos.com/emacs/modus-themes#h:c4b10085-149f-43e2-bd4d-347f33aee054
(use-package modus-themes
  :config
  ;;  modus-themes-italic-constructs t
  ;;  modus-themes-bold-constructs t
  (load-theme 'modus-vivendi :no-confirm))


;;; Garbage collection
(setq garbage-collection-messages nil)


;; Garbage collect only when emacs idles
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


;; Warn when opening >100MB files
(setq large-warning-threshold 100000000)


;;; Common lisp; see https://www.emacswiki.org/emacs/CommonLispForEmacsqq
;; (require 'cl-lib)


;;; Log command keystrokes into a separate buffer for presentations
;; (use-package command-log-mode
;;   :config
;;   (setq command-log-mode-auto-show t)
;;   (setq command-log-mode-window-size 60))
;; ;; (add-hook 'LaTex-mode-hook 'command-log-mode)
;; ;; To see the log buffer, call M-x clm/open-command-log-buffer


;;; Save file on window focus change
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))


;;; Benchmark initialization of Emacs
(emacs-init-time)


;;; Nicer (Doomlike) modeline (gonsie.com/blorg)
(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)


;;; Load defun files in separate library
(load "~/.emacs.d/library.el")


;;; Locating recently edited files
;; M-x recentf-open-files
;; Consider using the shortcut
(recentf-mode 1)


;;; Lorem-ipsum
(use-package lorem-ipsum
  :config
(global-set-key (kbd "C-c C-l s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c C-l p") 'lorem-ipsum-insert-paragraphs)
(global-set-key (kbd "C-c C-l l") 'lorem-ipsum-insert-list))


;;; Orderless provides a completion style that divides the pattern into space-separated components and matches candidates that match all the components in any order.
;; (use-package orderless
;;   :init (icomplete-mode)
;;   :custom (completion-styles '(orderless)))


;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;;  (setq split-height-threshold 0)
;;  (setq split-width-threshold 0)


;;; Use secure connections
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))


;;; Calculator
(use-package calc
  :ensure nil
  :custom
  (calc-highlight-selections-with-faces t)
  :bind
  ("C-M-=" . #'calc)
  ("M-#" . #'quick-calc)
  ("M-~" . #'calc-embedded))


;;; helpful: better contextual help
(use-package helpful
  ;;:disabled
  :commands (helpful-callable
             helpful-key
             helpful-variable
             helpful-symbol)
  :bind
  (("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
   :init
   (defalias #'describe-key #'helpful-key)
   (defalias #'describe-function #'helpful-callable)
   (defalias #'describe-variable #'helpful-variable)
   (defalias #'describe-symbol #'helpful-symbol))


;;; Calendar and Diary
;; List functions called whenever a date is visible
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq calendar-mark-holidays-flag t)


;;; Install org
(use-package org
:ensure nil
  :config
  (add-hook 'org-mode-hook
            #'(lambda ()
               (visual-line-mode 1)))(add-to-list 'org-emphasis-alist
            '("*" (:foreground "light blue")
              '("~" (:foreground "blue"))
              ))
  :bind
  (("C-c l" . org-insert-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . ibuffer) ; List of buffers
   ("C-c s" . eshell)  ; Shell
   ("C-c w" . eww)     ; Web browser
   ("C-c r" . indent-region)))     ; auto-indent region



;; Shift selection with arrows in org
(setq org-support-shift-select t)



;;; User settings
(setq user-full-name "Bruce Russell"
      user-mail-address "brussell8@icloud.com")


;;; Load custom file to enhance init.el intelligibility
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



;;; Auto-update packages
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe)
;;  (add-hook 'auto-package-update-before-hook
;;            (lambda () (message "Updating packages now.")))
  (message "%s" (propertize "foo" 'face '(:foreground "red")))
  (setq auto-package-update-delete-old-versions t))

;; Load the newer of the two if there are two of the same files to load
(setq load-prefer-newer t)

;;; Fill in some explanations for Emacs built-in prefixes
(use-package which-key
  :diminish
  :config
  (put 'which-key-add-key-based-replacements 'lisp-indent-function 0)
  (which-key-add-key-based-replacements
    "C-x 4" "other window"
    "C-x 5" "other frame"
    "C-x 8" "special characters"
    "C-x n" "narrow"
    "C-x r" "register/rectangle"
    "C-x a" "abbreviations"
    "C-x p" "pop-to-mark"
    "C-x t" "timeclock"
    "C-c o" "organizer.org"
    "C-c n" "atoms.org"
    "C-c j" "journal.org"
    "C-c k" "zettel"
    "C-c i" "init.el"
    "C-c !" "flycheck"
    "C-c /" "google"
    "C-c C-x" "org commands"
    "C-c b" "list of buffers"
    "C-c m" "mail client"
    "C-c s" "eshell"
    "C-c x" "today.org"
    "C-c w" "eww"
    "C-c n" "indent-region")
  (which-key-mode)
  (setq which-key-idle-delay 0.5))


;; posframe (usefulness?)
(use-package posframe)


;; Allow which-key to appear as a posframe
(use-package which-key-posframe
  :config
  (which-key-posframe-mode))


;;; Pop to mark
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)


;;; Sort apropros help results by relevancy
(setq apropos-sort-by-scores t)


;;; Undo-tree mode -- visualize undos and branches
;; see https://tech.toryanderson.com/2022/10/14/how-to-use-package-custom-with-a-variable-in-an-alist/
;; 'C-x u' . undo-tree-visualize
;; C-_  C-/  (`undo-tree-undo')
;;   Undo changes.
;; M-_  C-?  (`undo-tree-redo')
;;   Redo changes.
;; `undo-tree-switch-branch'
;;   Switch undo-tree branch.
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.
;; C-x r u  (`undo-tree-save-state-to-register')
;;   Save current buffer state to register.
;; C-x r U  (`undo-tree-restore-state-from-register')
;;   Restore buffer state from register.
;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))


;;; Default settings

;; Keybinding for M-x imenu
(global-set-key (kbd "M-i") 'imenu)

;; Open files in an existing frame
(setq ns-pop-up-frames nil)

;; Resize new window space equitably from all other windows
(setq-default window-combination-resize t)

;; Show columns in mode line
(setq column-number-mode t)

;; Show battery status on laptops
(display-battery-mode 1)

;; Silence error bell
(setq visible-bell t)


;; Set frame size and column width
(setq-default fill-column 80)
(setq initial-frame-alist
      '((width . 90)
	(height . 54)))
(setq default-frame-alist
      '((width . 90)
	(height . 54)))


;;; Eliminate unnecessary terminal key bindings C-m, C-i, and C-[; we don't need this in GUI so we can free up these keys

(add-hook
 'after-make-frame-functions
 (defun setup-blah-keys (frame)
   (with-selected-frame frame
     (when (display-graphic-p)
       (define-key input-decode-map (kbd "C-i") [C-i])
       (define-key input-decode-map (kbd "C-[") [C-lsb])
       (define-key input-decode-map (kbd "C-m") [C-m])))))


;;; Personal setting for bibliographical information
(defvar my/global-bibliography '("~/.emacs.d/lib.bib"))


;; Delete with ~M-x delete-trailing-whitespace RET~
(setq show-trailing-whitespace t)


;; Avert non-graphic display error with ~when~ expression
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))


;; Show end of buffer with special glyph in left fringe
(setq-default indicate-empty-lines t)


;; Reduce cursor lag
(setq auto-window-vscroll nil)


;; Keep point at position when scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 1)
(setq scroll-margin 0)


;; Divide portmanteau tech words by syllable (e.g., EmacsFrameClass)
(setq global-subword-mode t)


;; Various appearance items
(global-prettify-symbols-mode t)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(setq org-hide-emphasis-markers t)      ; hide the *,=, or / markers


;; Have \alpha,\to and others display as utf8
(setq org-pretty-entities t)
(setq inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message ""
      )
(setq require-final-newline t)
;; let Emacs put in a return for you after left curly braces, right curly braces, and semi-colons.
;; (setq c-auto-newline 1)


;; .tex files should be handled by latex-mode
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))


;; Do not kill scratch buffer (minor mode)
(use-package unkillable-scratch
  :config (unkillable-scratch t))


;; Single-space after sentence end
(setq sentence-end-double-space nil)


;; Show full path in title bar
(setq-default frame-title-format "%b (%f)")


;; Abbreviate yes-or-no as 'y-or-n'
(setq use-short-answers t)
(when (version< emacs-version "28")
  (fset 'yes-or-no-p 'y-or-n-p))


;; Highlight current cursor line
(global-hl-line-mode t)


;; Shift arrow switches windows
(use-package windmove)
(windmove-default-keybindings)


;; Color fonts for coding
(global-font-lock-mode t)


;; Update buffers on external file change
;; This slows down Emacs; will ask first
(global-auto-revert-mode t)
;; Reverts dired buffers
(setq global-auto-revert-non-file-buffers t)


;; Commands use minibuffer while minibuffer is active; sometimes you want to use the minibuffer in the minibuffer, for example, you query-replace and you want to replace something with insert-char (with completion) (understanding?)
(setq enable-recursive-minibuffers t)


;; Case-sensitive searching
(setq-default case-fold-search nil)


;; Narrow-to-region without confirmation
;; 'C-x n n'; 'C-x n w' re-widens
(put 'narrow-to-region 'disabled nil)


;; Wrap lines always and allow editing (rebinds C-a, C-e, & C-k) by visual lines
(global-visual-line-mode 1)


;; Beacon briefly highlights cursor on window change
(use-package beacon
  :diminish
  :init
  (beacon-mode 1))


;;; Prefer file encoding system UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)


;;; Tramp for remote file editing with default configuration
(setq tramp-default-method "ssh"
       tramp-backup-directory-alist backup-directory-alist
       tramp-ssh-controlmaster-options "ssh")


;;; Fonts (N.B. Command+T opens font menu on OS X)

;; Enable emoji
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


;; Make fonts look better with anti-aliasing.
(setq mac-allow-anti-aliasing t)


;;; Smart mode line package
(use-package smart-mode-line)
(setq sml/no-confirm-load-theme t)
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))


;;; Backups kept without cluttering up folders
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup")))


;; Back up files when first saved
(setq make-backup-files t
;; Don't clobber symlinks
      backup-by-copying t
;; Version numbers for backup files
      version-control t
;; Disk space is cheap, so we can change this to -1 and delete kept-x-versions
      delete-old-versions t
;; Back up versioned files
      vc-make-backup-files t
      kept-new-versions 6
      kept-old-versions 2)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))


;;; Save history (M-n: next-history-element; M-p: previous-history-element)
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length 25)
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))


;;; Recenter buffer in quarters with C-l
(setq recenter-positions '(0.25 0.5 0.75))


;;; Mac-specific: delete to Trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")



;;; Dired; N.B. :custom
;;(dired-listing-switches "-aBhl --group-directories-first")) caused a "listing directory failed but 'access-file' worked" error message
(use-package dired
:ensure nil
  :config
(when (eq system-type 'darwin)
(setq insert-directory-program "/usr/local/bin/gls")))


;; find-dired library faster search; see https://www.masteringemacs.org/article/working-multiple-files-dired
;; use find-name-dired for wildcard search
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;;; All-the-icons
(use-package all-the-icons)


(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))


;; Search only among file names
(setq dired-isearch-filenames t)


;;; Select help window on open
(setq help-window-select t)


;;; Delete selection with backspace
(delete-selection-mode 1)
;; Disable "hello file" function
(global-unset-key (kbd "C-h h"))


;;; Mac modifiers
(setq ns-command-modifier 'super
      ns-right-command-modifier 'super
      ns-alternate-modifier 'meta
      mac-function-modifier 'hyper ; make Fn key Hyper
      ns-use-native-fullscreen nil)


;; To use right-option to write accents, etc.
 (when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))


;; If more than one window, show tabs (1 means if there are more than one tab)
(setq tab-bar-show 1)


;;; No tabs; only spaces (tabs screw up TeX)
(setq-default tab-width 4
              indent-tabs-mode nil)
0(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)


;;; Set path for private config
(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))


;;; Save cursor location and return to it on reopen
(save-place-mode 1)


;;; Insert and highlight parenthesis and bracket pairs; consider Paredit
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-foreground 'show-paren-match nil)


;;; Smartparens (https://github.com/Fuco1/smartparens)
(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (smartparens-global-mode))



;; Disable Smartparens in Org-mode
  (add-to-list 'sp-ignore-modes-list #'org-mode)
  (add-to-list 'sp-ignore-modes-list #'org-agenda-mode)


;; Rainbow delimiters (https://github.com/Fanael/rainbow-delimiters)
;; Highlight delimiters like parentheses in rainbow fashion
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


;;; Enable copy-pasting outside of Emacs
(setq x-select-enable-clipboard t)


;;; PDF files
(setq doc-view-continuous t)


;;; Org mode

;; Giving org-mode a modern look; see https://github.com/minad/org-modern
(use-package org-modern)
;; (add-hook 'org-mode-hook #'org-modern-agenda) ; per buffer
(global-org-modern-mode) ; globally


;; Update table of contents (TOC) on save
(use-package org-make-toc
  :hook
  (org-mode . org-make-toc))


;; Substitute arrow for org ellipses
(setq org-ellipsis " ▾")


;;; Org-mode
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/")
;; Currently only one agenda file--to revert, change to /org/
(setq org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/organizer.org"))
;; (setq org-agenda-files (list org-directory))
(add-hook 'org-agenda-after-show-hook #'recenter)
;;(setq org-default-notes-file (concat org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/atoms.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
;; Auto-save org buffers to disk while Emacs runs
(add-hook 'auto-save-hook 'org-save-all-org-buffers)


;;; gdb (GNU debugger) multi-windowed
(setq gdb-many-windows t
      gdb-show-main t)


;;; Link to zettels in The Archive
;; (org-link-set-parameters
;;    "zettel"
;; ;; Don't fold links but show ID and description:
;; [[zettel:202102101021][Title or description here]]”
;;   :display 'full)
;; :follow (lambda (searchterm)
;;           (browse-url (concat "thearchive://match/" searchterm)))


;;; Custom agenda features; see [[https://orgmode.org/manual/Agenda-Commands.html]]
(custom-set-variables
 '(org-agenda-span 14)
 '(org-deadline-warning-days 5)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-schedule-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode t)) ; show log in agenda view


;; Retain window splits
(setq org-agenda-restore-windows-after-quit t)

;; Include diary in agenda view
(setq org-agenda-include-diary t)


;;; Task list categories
;; Prompt for fast-access key
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "STUDY(s)" "WRITE(r)" "WAIT(w)" "DELEGATE(e)" "|" "DONE(d)" "CANCELED(x)")))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red")
              ("NEXT" :foreground "blue")
              ("PROJ" :foreground "purple")
              ("STUDY" :foreground "dark cyan")
              ("WRITE" :foreground "dark magenta")
              ("WAIT" :foreground "orange")
              ("DELEGATE" :foreground "cyan")
              ("DONE" :foreground "forest green")
              ("CANCELED" :foreground "light sea green"))))


;;; Log task completion date
(setq org-log-done 'time)


;;; Log notes into drawer
;; (setq org-log-into-drawer ;TODO:)


;;; Prevent project children from appearing as "projects"
(setq org-tags-exclude-from-inheritance '("project"))


;;; Mark all child tasks "Done" before marking parent "Done"
(setq org-enforce-todo-dependencies t)


;;; Use normal TAB function and syntax highlighting inside of code blocks
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)


;;; Color code org priority (A,B,C) faces
(setq org-priority-faces '((65 :foreground "#e45649" :weight bold)
                           (66 :foreground "#da8548")
                           (67: foreground "#0098dd")))


;;; Capture templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/organizer.org" "Inbox")
         "* TODO %?\n %U\n %a\n %i" :empty-lines 1); consider file+olp
        ("l" "Link" entry (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/links.org" "Links")
         "* %:description\n%u\n\n%c\n\n%i" :empty-lines 1)
        ("j" "Journal" entry (file+olp+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/journal.org")
         "* %?" :unnarrowed t)
        ("b" "Bookmark" entry (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/bookmarks.org" "Bookmarks")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("n" "Note" entry (file+olp+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/atoms.org") "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%1\n\n- %a" :prepend t)
        ("p" "Daily Plan" plain (file+olp+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/today.org") (file "~/Dropbox/org/tpl-dailyplan.txt"))
        ;;        ("d" "Diary" entry (file+olp+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/diary.org") "*  ;;%?\n%U\n" :clock-in t :clock-resume t)
        ))

;; Show path when refiling
(setq org-refile-use-outline-path 'file)


;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)


;;; Export with smart quote marks
(setq org-export-with-smart-quotes t)


;;; Export without contact information in footer
(setq org-html-postamble nil)


;;; Narrow or widen org subtrees with Cmd+Ctrl+Up/Down
(define-key org-mode-map (kbd "C-s-<down>") 'org-narrow-to-subtree)
(define-key org-mode-map (kbd "C-s-<up>") 'widen)


;;; Global set keys for commonly used files and functions
(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c j")
                (lambda () (interactive) (find-file "~~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/journal.org")))
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/organizer.org")))
(global-set-key (kbd "C-c x")
                (lambda () (interactive) (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/today.org")))


;;; Flip between current and previous buffer
(global-set-key [f6] (lambda () (interactive) (switch-to-buffer nil)))


;;; Google translate for language learning: select phrase, then =detect language= see [[https://alhassy.github.io/init/][alhassy]]
(use-package google-translate
  :config
  (global-set-key "\C-c t" 'google-translate-at-point)
  )

;;; Writing

;;; Define word at point: M-\\
(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))


(setq dictionary-server "dict.org")

;;; Markdown (see https://jblevins.org/projects/markdown-mode/)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;; Markdown editor; run `M-x grip-mode` to preview the markdown file in the default browser
(use-package grip-mode
  :bind (:map markdown-mode-command-map
	      ("g" . grip-mode)))


;;; Olivetti centered buffer content for distraction-free writing
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun my/distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
    (delete-other-windows)
    (text-scale-increase 0.5)
    (olivetti-mode t))
  (progn
    (olivetti-mode 0)
    (text-scale-decrease 0.5)
    )))
  :bind
  ("<f9>" . my/distraction-free))


;;; Abbreviations
;; M-x abbrev-mode turns it on and off
;; M-x add-global-abbrev defines at the prompt [add-mode-abbrev for a particular mode]
;; M-x universal-argument C-u 0 M-x add-global-abbrev for multi-word abbrev., after selecting several words, define at the prompt
;; M-x universal-argument C-u -1 M-x add-global-abbrev to remove abbrev.
;; Mx list-abbrevs for a list of defined abbrevs.
;; M-x edit-abbrevs to add or remove or edit abbrevs.
(setq save-abbrevs t)
;; Save abbreviations on file save
(setq save-abbrevs 'silently)
;; Turn on abbrev-mode globally
(setq-default abbrev-mode t)
;; Abbreviation definitions file
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")
(if (file-exists-p abbrev-file-name) (quietly-read-abbrev-file))
(setq abbrev-suggest-hint t)
(setq abbrev-suggest-hint-threshold 5)
;; Show all abbreviations made during this edit
(setq abbrev-suggest-show-report t)


;;; magit
;;(use-package magit
;;  :config
;;  (global-set-key (kbd "s-g") 'magit-status))


;; Show changes in the gutter (fringe).
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode 't)
  (set-face-background 'git-gutter:modified 'nil) ;; background color
  (set-face-foreground 'git-gutter:added "green4")
      (set-face-foreground 'git-gutter:deleted "red"))


;; Page through file history with git-timemachine
(use-package git-timemachine)


;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Erc" (mode . erc-mode))
               ("Eww" (mode . eww-mode))
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (or
                        (mode . mu4e-compose-mode)
                        (name . "\*mu4e\*")
                        ))
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\$")
                         (name . "^\\*Messages\\*$")))
               ("Help" (or (name . "\*Help\*")
                           (name . "\*Apropos\*")
                           (name . "\*Info\*")
                           ))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Omit filter groups lacking buffers

(setq ibuffer-show-empty-filter-groups nil)
;; Delete marked buffers without confirmation
;; (setq ibuffer-expert t)


;;; Open links in modes other than org, e.g., Markdown
(add-hook 'text-mode-hook (lambda ()
                            (goto-address-mode)))


;; Completion
;; Swiper, Ivy  & Counsel (toggle Ivy with M-x ivy-mode)

;; Counsel provides a superset of functions for navigating the file system switching buffers, etc., that expand on basic features supported by ivy--for instance, switching buffers with counsel provides a preview of their contents in the window.

(use-package counsel)

;; Ivy handles all selection lists, narrowing them down using a variety of possible builders (regular expressions of flexible matching).
(use-package ivy
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)      ; Show bookmarks and recent files in buffer list
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Swiper is a tool for performing searches powered by ivy, while presenting a preview of the results.
(use-package swiper
  :bind (("C-S-s" . swiper-isearch)
         ("C-S-r" . swiper-isearch)
         ("C-c C-r" . ivy-resume))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)))

;;; Company-mode auto-completion (M-x company-mode)
(use-package company
  :diminish
  :config
  ;; Trigger completion immediately
  ;;	  (setq company-idle-delay 0)
  ;; Trigger completion after delay
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 3)
  ;; Number the candidates (use M-1, M-2, etc. to select completions).
  (setq company-show-numbers t)
  (global-company-mode t))

;; vertico, embark as replacement for ivy, counsel
;; Enable vertico
;; (use-package vertico
;; :init
;; (vertico-mode))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  ;; )

;;; Shell (consider vterm)
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Alternative expression--what are the differences?
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns))
;;   :config
;;   (exec-path-from-shell-initialize))


;;; Sensible ediff behavior (UNIX patching tool)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)


;;; Highlight uncommitted changes
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))


;;; Dogears: "This library automatically and smartly remembers where you've
;; been, in and across buffers, and helps you quickly return to any of those
;; places. It uses the Emacs bookmarks system internally, but without
;; modifying the bookmarks-alist, to save and restore places with
;; mode-specific functionality."

(use-package dogears
   :bind (:map global-map
      ("M-g d" . dogears-go) ; go to a dogeared place with completion
      ("M-g M-b" . dogears-back) ; previous dogeared place
      ("M-g M-f" . dogears-forward) ; next dogeared place
      ("M-g M-f" . dogears-list) ; show dogeared places in list
      ("M-g M-D" . dogears-sidebar))) ; show list in side window


;;; Debug on error
(setq debug-on-error t)
;; If using pair-tree, then: 
;; (setq debug-on-error nil)

;;; yasnippet; see overview https://joaotavora.github.io/yasnippet/index.html
(use-package yasnippet
  :config (use-package yasnippet-snippets) (yas-reload-all))
 (yas-global-mode 1)


;;; Erc, InternetRelayChat (IRC); see [[https://www.emacswiki.org/emacs/ErcBasics][ErcBasics]]
;; Direct notices appear in the minibuffer
(setq erc-echo-notices-in-minibuffer-flag t)


;;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")

;; Popup window for spellchecking; n.b., flyspell-auto-correct-previous-word bound to C-;
;; See https://www.emacswiki.org/emacs/FlySpell
(use-package flyspell-correct
  :config
  :diminish)

;; Enable spellcheck on the fly for all text modes, including LaTeX
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Spellcheck current word
(define-key flyspell-mode-map (kbd "s-\\")
  'flyspell-correct-previous-word-generic)
;; Cmd+\ spellcheck word with popup
(define-key flyspell-mode-map (kbd "C-s-\\") 'ispell-word)
;; Ctrl+Cmd+\ spellcheck word using built UI



;;; Math symbols hydra

(use-package hydra)


(use-package pretty-hydra)

;; Hydra - Lower Case

(defhydra math-lowercase (:color blue :columns 6)
  "Select your symbol below (also try C-c u)"
  ("a" (lambda () (interactive) (insert "α")) "α")
  ("b" (lambda () (interactive) (insert "β")) "β")
  ("c" (lambda () (interactive) (insert "∊")) "∊")
  ("d" (lambda () (interactive) (insert "δ")) "δ")
  ("e" (lambda () (interactive) (insert "ε")) "ε")
  ("f" (lambda () (interactive) (insert "φ")) "φ")
  ("g" (lambda () (interactive) (insert "γ")) "γ")
  ("h" (lambda () (interactive) (insert "θ")) "θ")
  ("i" (lambda () (interactive) (insert "ι")) "ι")
  ("j" (lambda () (interactive) (insert "ξ")) "ξ")
  ("k" (lambda () (interactive) (insert "κ")) "κ")
  ("l" (lambda () (interactive) (insert "λ")) "λ")
  ("m" (lambda () (interactive) (insert "μ")) "μ")
  ("n" (lambda () (interactive) (insert "η")) "η")
  ("o" (lambda () (interactive) (insert "ο")) "ο")
  ("p" (lambda () (interactive) (insert "π")) "π")
  ("r" (lambda () (interactive) (insert "ρ")) "ρ")
  ("s" (lambda () (interactive) (insert "σ")) "σ")
  ("t" (lambda () (interactive) (insert "τ")) "τ")
  ("u" (lambda () (interactive) (insert "υ")) "υ")
  ("v" (lambda () (interactive) (insert "ν")) "ν")
  ("w" (lambda () (interactive) (insert "ω")) "ω")
  ("x" (lambda () (interactive) (insert "χ")) "χ")
  ("y" (lambda () (interactive) (insert "ψ")) "ψ")
  ("z" (lambda () (interactive) (insert "ζ")) "ζ")
  ("," (lambda () (interactive) (insert "≤")) "≤")
  ("." (lambda () (interactive) (insert "≥")) "≥")
  ("=" (lambda () (interactive) (insert "≠")) "≠")
  ("-" (lambda () (interactive) (insert "±")) "±")
  ("0" (lambda () (interactive) (insert "∅")) "∅")
  ("1" (lambda () (interactive) (insert "→")) "→")
  ("2" (lambda () (interactive) (insert "↔")) "↔")
  ("3" (lambda () (interactive) (insert "↦")) "↦")
  ("4" (lambda () (interactive) (insert "↑")) "↑")
  ("5" (lambda () (interactive) (insert "↓")) "↓")
  ("6" (lambda () (interactive) (insert "↗")) "↗")
  ("7" (lambda () (interactive) (insert "↘")) "↘")
  ("8" (lambda () (interactive) (insert "∞")) "∞")
  ("9" (lambda () (interactive) (insert "⋯")) "⋯")
  ("q" nil "cancel"))

;; Hydra - Upper Case

(defhydra math-uppercase (:color blue :columns 6)
  "Select your symbol below (also try C-c m)"
  ("a" (lambda () (interactive) (insert "∀")) "∀")
  ("b" (lambda () (interactive) (insert "∃")) "∃")
  ("c" (lambda () (interactive) (insert "ℂ")) "ℂ")
  ("d" (lambda () (interactive) (insert "Δ")) "Δ")
  ("e" (lambda () (interactive) (insert "∈")) "∈")
  ("f" (lambda () (interactive) (insert "Φ")) "Φ")
  ("g" (lambda () (interactive) (insert "Γ")) "Γ")
  ("h" (lambda () (interactive) (insert "Θ")) "Θ")
  ("i" (lambda () (interactive) (insert "∫")) "∫")
  ("j" (lambda () (interactive) (insert "∂")) "∂")
  ("k" (lambda () (interactive) (insert "⊢")) "⊢")
  ("l" (lambda () (interactive) (insert "Λ")) "Λ")
  ("m" (lambda () (interactive) (insert "∄")) "∄")
  ("n" (lambda () (interactive) (insert "ℕ")) "ℕ")
  ("o" (lambda () (interactive) (insert "⊕")) "⊕")
  ("p" (lambda () (interactive) (insert "Π")) "Π")
  ("r" (lambda () (interactive) (insert "ℝ")) "ℝ")
  ("s" (lambda () (interactive) (insert "Σ")) "Σ")
  ("t" (lambda () (interactive) (insert "∴")) "∴")
  ("u" (lambda () (interactive) (insert "∵")) "∵")
  ("v" (lambda () (interactive) (insert "√")) "√")
  ("w" (lambda () (interactive) (insert "Ω")) "Ω")
  ("x" (lambda () (interactive) (insert "∊")) "∊")
  ("y" (lambda () (interactive) (insert "Ψ")) "Ψ")
  ("z" (lambda () (interactive) (insert "ℤ")) "ℤ")
  ("," (lambda () (interactive) (insert "¬")) "¬")
  ("." (lambda () (interactive) (insert "≡")) "≡")
  ("=" (lambda () (interactive) (insert "≈")) "≈")
  ("-" (lambda () (interactive) (insert "≠")) "≠")
  ("0" (lambda () (interactive) (insert "∉")) "∉")
  ("1" (lambda () (interactive) (insert "ℚ")) "ℚ")
  ("2" (lambda () (interactive) (insert "⊂")) "⊂")
  ("3" (lambda () (interactive) (insert "⊃")) "⊃")
  ("4" (lambda () (interactive) (insert "⋂")) "⋂")
  ("5" (lambda () (interactive) (insert "⋃")) "⋃")
  ("6" (lambda () (interactive) (insert "∧")) "∧")
  ("7" (lambda () (interactive) (insert "∨")) "∨")
  ("8" (lambda () (interactive) (insert "∙")) "∙")
  ("9" (lambda () (interactive) (insert "∘")) "∘")
  ("q" nil "cancel"))


;; Key Maps

(global-set-key (kbd "C-c v") #'math-lowercase/body)
(global-set-key (kbd "C-c u") #'math-uppercase/body)


;;; Google
(use-package google-this
  :diminish
  :config
  (google-this-mode 1))

;; The main function is `google-this' (bound to C-c / g). It does a
;; google search using the currently selected region, or the
;; expression under point. All functions are bound under "C-c /"
;; prefix, in order to comply with Emacs' standards. If that's an
;; issue, see `google-this-keybind'. To view all keybindings type "C-c
;; / C-h".


;;; Timeclock
;; If you change your timelog file without using timeclock's
;; functions, or if you change the value of any of timeclock's
;; customizable variables, run command `timeclock-reread-log'.
(require 'timeclock)

(define-key ctl-x-map "ti" 'timeclock-in)
(define-key ctl-x-map "to" 'timeclock-out)
(define-key ctl-x-map "tc" 'timeclock-change)
(define-key ctl-x-map "tr" 'timeclock-reread-log)
(define-key ctl-x-map "tu" 'timeclock-update-mode-line)
(define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

(add-hook 'kill-emacs-query-functions 'timeclock-query-out)


;;; Helm completion system

;; (use-package helm
;;   :config
;;   (require 'helm-config)
;;   :init
;;   (helm-mode 1)
;;   :bind
;;   (("M-x"     . helm-M-x)
;;    ("C-x C-f" . helm-find-files)
;;    ("C-x b"   . helm-mini)
;;    ("C-x C-r" . helm-recentf)
;;    ("C-c i"   . helm-imenu)
;;    ("M-y"     . helm-show-kill-ring)
;;    :map helm-map
;; ;;   ("C-z" . helm-select-action)
;;   ("<tab>" . helm-execute-persistent-action)))



;;; Insert a block of elisp

(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))

(global-set-key (kbd "C-c C-,") #'org-insert-structure-template)


;;; View mode only for read-only files and files set read-only with C-x C-q
;; Provide pager-like keybindings. Makes navigating read-only buffers a breeze. Move down and up with SPC and delete (backspace) or S-SPC, half a page down and up with d and u, and isearch with s.
;; M-x view-mode
(setq view-mode-only t)


;; ;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-and-add-front-matters))


;;; init.el ends here
