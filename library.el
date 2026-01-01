;;; library.el --- defuns library

;;; Code:

;;; Rename file and buffer it's visiting
(defun my/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to new-name." (interactive "new name:")
  (let ((name(buffer-name))
        (filename(buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s'' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named %s' already exists!" new-name)
        (progn (rename-file filename new-name 1) (rename-buffer new-name) (set-visited-file-name new-name) (set-buffer-modified-p nil))))))


;;; Toggle line numbers
(defun my/enable-line-numbers ()
  "Display line numbers"
  (interactive)
  (setq display-line-numbers t))


(defun my/disable-line-numbers ()
  "Disable line numbers"
  (interactive)
  (setq display-line-numbers nil))

(add-hook 'org-mode-hook #'my/disable-line-numbers)


;;; Align tags in buffer on tag changes
(defun my/org-align-all-tags ()
  "Aligns all org tags in the buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-align-tags t)))

(add-hook 'org-after-tags-change-hook #'my/org-align-all-tags)


;;; Copy a link to clipboard

;; (defun my/copy-idlink-to-clipboard ()
;;   "Copy an ID link with the headline to the killring. If no ID is there, then create a new unique ID. This function works only in org-mode or org-agenda buffers.

;; The purpose of this function is to easily construct id:-links to org-mode items. If it's assigned to a key, it saves marking and copying the text to the killring."
;;   (interactive)
;;   (when (eq major-mode 'org-agenda-mode) ; switch to orgmode
;;     (org-agenda-show)
;;     (org-agenda-goto))
;;   (when (eq major-mode 'org-mode)  ; do this only in org-mode buffers
;;     (setq mytmphead (nth 4 (org-heading-components)))
;;     (setq mytmpid (funcall 'org-id-get-create))
;;     (setq mytmplink (form "[[id:%s][%s]]" mytmpid mytmphead))
;;     (kill-new mytmplink)
;;     (message "Copied %s to killring (clipboard)" mytmplink)
;;     ))

;; (global-set-key (kbd "<f5>") 'my/copy idlink-to-clipboard)


;;; Insert current date
(defun my/insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%Y-%m-%d')")))


;; Company completion vocabulary sets to css and html in web-mode
(defun my/web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))


;; Emacs find HERE
(defun find-next-here-mark ()
  "Find the string HERE in the current buffer."
  (let ((case-fold-search nil))
    (search-forward "HERE" nil t)))
(add-hook 'find-file-hook #'find-next-here-mark)


;; M-w kills an entire single line without setting a mark

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Single line killed")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; C-w copies an entire single line without setting a mark

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	       (line-beginning-position 2)))))

;; A smart way to close files (showing diff option); see https://in.comum.org/smart-way-to-close-files.html

;; The snippet creates a function that checks if the buffer you are currently in, was modified since its file was last read or saved and prompts you four options: quit, save, set as not modified, show the difference between current and previous state. After you choose the option, it will kill the current buffer.

(defun my/kill-this-buffer ()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q) (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        (kill-buffer (current-buffer))))))

(global-set-key (kbd "C-x k") 'my/kill-this-buffer)


;; Split new window below all other windows
;; (defun my/split-below (arg)
;;   "Split window below from the parent or from root with ARG."
;; (interactive "P")
;; (split-window (if arg (frame-root-window)
;;                 window-parent (selected-window)))
;;nil 'below nil)

;; Jump to last change in buffer
(defun my/back-to-last-edit ()
  "Jump back to the last change in the current buffer."
  (interactive)
  (ignore-errors
    (let ((inhibit-message t)) ;; silence messages for undo & redo
      (undo-only)
      (undo-redo))))

;; Reopen the last buffer killed
(defun my/undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))


;; Insert date stamp
;;(defun my/org-insert-current-time-as-inactive-time-stamp ()
;;   (interactive)
;;   (insert (format-time-string "[%Y-%m-%d]")))

;; (define-key org-mode-map (kbd "C-c C-_") ;;#'my/org-insert-current-time-as-inactive-time-stamp)

;; Reload Emacs

(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


;; Find changed words in file buffer with command line diff tool

(defun my-wdiff (old-file new-file)
  (interactive (list (read-file-name "Original: ")
                     (buffer-file-name)))
  (with-current-buffer (get-buffer-create "*wdiff*")
    (erase-buffer)
    (call-process "wdiff" nil t t old-file new-file)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[-\\|{\\+\\)\\(.*?\\)\\(-\\]\\|\\+}\\)" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list 'face (if (string= (match-string 1) "[-")
                                             'diff-removed
                                           'diff-added))))
    (switch-to-buffer (current-buffer))))

(defun my-wdiff-buffer-with-file ()
  (interactive)
  (let ((s (buffer-string))
        (temp-file (make-temp-file "temp")))
    (with-temp-file temp-file
      (insert s))
    (my-wdiff (buffer-file-name) temp-file)
    (delete-file temp-file)))
