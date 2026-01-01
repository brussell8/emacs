;;; package -- custom variables

;;; Commentary: A segregated file to avoid cluttering init.el.

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer) t)
 '(browse-url-browser-function 'browse-url-default-macosx-browser)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-normal-cursor-color "#8d8b86")
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("eb7cd622a0916358a6ef6305e661c6abfad4decb4a7c12e73d6df871b8a195f8"
     "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7"
     "5fdc0f5fea841aff2ef6a75e3af0ce4b84389f42e57a93edc3320ac15337dc10"
     "3ab376acffab6b4e79ae2b6e0a1cce3fa21dbac0027f0ff0dfef02b5c838dba9"
     "74a50f18c8c88eac44dc73d7a4c0bbe1f3e72ff5971aac38fcf354ddad0d4733"
     "6fc03df7304728b1346091dd6737cb0379f348ddc9c307f8b410fba991b3e475"
     "7397cc72938446348521d8061d3f2e288165f65a2dbb6366bb666224de2629bb"
     "7afad8f4707c84129e4cb1e5bad4feb0a9f0db02e1cfadb029921a0bde693d1e"
     "6dc02f2784b4a49dd5a0e0fd9910ffd28bb03cfebb127a64f9c575d8e3651440"
     "31f1723fb10ec4b4d2d79b65bcad0a19e03270fe290a3fc4b95886f18e79ac2f"
     "e1ad4299390cb3fc0cbf5a705442eaf08510aa947c90c8bc83b1d7308befb475"
     "0568a5426239e65aab5e7c48fa1abde81130a87ddf7f942613bf5e13bf79686b"
     "076ee9f2c64746aac7994b697eb7dbde23ac22988d41ef31b714fc6478fee224"
     "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f"
     "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75"
     "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358"
     "e8830baf7d8757f15d9d02f9f91e0a9c4732f63c3f7f16439cc4fb42a1f2aa06"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "b79919597ed10c79a04ed7bd75f61a1af58c84aa13e2467ea8daa7b5d584838a"
     "17be36b47116bedc7659d7ce4e037fb9bc41c83bf5f573e490f455f5c72678de"
     "ffc63d99923d5923fc6a3afa59f6a41a5f21f0574fef80ec17a29c393f684daf"
     "041eda6544d300ec0cfcca361929033ad41554753ac2f92a9d49bd6f3e1023b3"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-mode t)
 '(ensime-sem-high-faces
   '((var :foreground "#9876aa" :underline (:style wave :color "yellow"))
     (val :foreground "#9876aa") (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline (:color "#808080"))
     (implicitParams :underline (:color "#808080"))
     (operator :foreground "#cc7832") (param :foreground "#a9b7c6")
     (class :foreground "#4e807d") (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832") (deprecated :strike-through "#a9b7c6")))
 '(fci-rule-color "#202325")
 '(highlight-symbol-colors
   '("#55204c0039fc" "#3f0a4e4240dc" "#5a2849c746fd" "#3fd2334a42f4"
     "#426a4d5455d9" "#537247613a13" "#46c549b0535c"))
 '(highlight-symbol-foreground-color "#999891")
 '(highlight-tail-colors
   '(("#01323d" . 0) ("#687f00" . 20) ("#008981" . 30) ("#0069b0" . 50)
     ("#936d00" . 60) ("#a72e01" . 70) ("#a81761" . 85) ("#01323d" . 100)))
 '(hl-bg-colors
   '("#4c4536" "#4b4136" "#504341" "#4d3936" "#3b313d" "#41434a" "#3b473c"
     "#3d464c"))
 '(hl-fg-colors
   '("#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29"
     "#2a2a29"))
 '(hl-paren-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(lsp-ui-doc-border "#999891")
 '(nrepl-message-colors
   '("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d464c" "#e3eaea" "#41434a" "#7ec98f"
     "#e5786d" "#834c98"))
 '(org-agenda-files '("~/Dropbox/org"))
 '(org-agenda-ndays 14)
 '(org-agenda-ndays-to-span 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-schedule-if-done t)
 '(org-agenda-span 14)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode t)
 '(org-deadline-warning-days 5)
 '(org-directory "~/Dropbox/org")
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(## 0blayout 0x0 ac-ispell aggressive-indent all-the-icons all-the-icons-dired
        all-the-icons-ivy auto-compile auto-package-update beacon bug-hunter
        command-log-mode company company-lsp company-tabnine counsel darkroom
        dashboard define-word deft denote desktop-environment diff-hl diminish
        dired dired-subtree dogears dracula-theme edit-indirect-region-latex
        elfeed elpy emmet-mode exec-path-from-shell flycheck flyspell-correct
        flyspell-correct-popup git-commit git-gutter git-timemachine
        golden-ratio google-this google-translate goto-chg goto-last-change
        grip-mode helm helm-dogears helpful htmlize hungry-delete iedit isearch
        ivy ivy-rich jedi js2-mode lorem-ipsum lsp-mode magit markdown-mode
        markdown-mode+ modus-themes mu4e-conversation mu4e-views multi-term
        multi-web-mode nano-agenda neotree olivetti orderless org org-ac
        org-appear org-bullets org-caldav org-journal org-mac-link org-make-toc
        org-modern org-pdfview org-plus-contrib org-remark
        org-remark-global-tracking org-roam org-roam-bibtex org-roam-server
        org-roam-ui org-superstar orgalist outline-minor-faces pandoc
        pandoc-mode pretty-hydra projectile rainbow-delimiters request
        restart-emacs simpleclip smart-mode-line smartparens
        solarized-dark-high-contrast solarized-theme toc-org undo-tree
        unkillable-scratch vterm which-key-posframe yasnippet
        yasnippet-classic-snippets zerodark-theme))
 '(pos-tip-background-color "#2f2f2e")
 '(pos-tip-foreground-color "#999891")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2f2f2e" 0.2))
 '(sp-escape-quotes-after-insert nil)
 '(term-default-bg-color "#2a2a29")
 '(term-default-fg-color "#8d8b86")
 '(weechat-color-list
   '(unspecified "#2a2a29" "#2f2f2e" "#504341" "#ffb4ac" "#3d464c" "#8ac6f2"
                 "#4c4536" "#e5c06d" "#41434a" "#a4b5e6" "#4d3936" "#e5786d"
                 "#3b473c" "#7ec98f" "#8d8b86" "#74736f")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
