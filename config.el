;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "R.P."
      user-mail-address "r.pezzotta1@campus.unimib.it")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "IBM Plex Mono" :size 17) ;; :weight 'regular)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 19 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documenti/emacs/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :leader "SPC" nil)

(map! :leader "t [" #'smartparens-mode)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(global-set-key (kbd "<menu>") 'save-buffer)

(define-key evil-insert-state-map (kbd "\C-e") 'evil-copy-from-below)

(map! :leader :desc "Open vterm popup" "o T" #'+vterm/toggle)
(map! :leader :desc "Open vterm here" "o t" #'+vterm/here)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq fancy-splash-image "~/Pictures/.emacs_mars.png")

(assoc-delete-all "Reload last session" +doom-dashboard-menu-sections)
(assoc-delete-all "Open org-agenda" +doom-dashboard-menu-sections)
(assoc-delete-all "Open project" +doom-dashboard-menu-sections)
(assoc-delete-all "Open documentation" +doom-dashboard-menu-sections)

(remove-hook! '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "I showed you my config files, pls respond")))

(add-hook 'after-init-hook #'display-battery-mode)
(add-hook 'after-init-hook #'display-time)
(setq display-time-24hr-format t)

(setq select-enable-clipboard nil)

(global-set-key (kbd "S-<insert>") 'clipboard-yank)
(define-key evil-visual-state-map (kbd "C-<insert>") 'clipboard-kill-region)

(defun robert/yank ()
  (interactive)
  (evil-use-register ?+)
  (call-interactively 'evil-yank))
(global-set-key (kbd "C-<insert>") 'robert/yank)

(add-to-list '+lookup-provider-url-alist '("Startpage" "https://www.startpage.com/sp/search?query=%s"))

(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)

(set-file-template! "/__\\.py$g" :trigger "__" :mode 'python-mode)

(defun open-file-externally ()
  "Open the current file's directory in external file browser."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (consult-file-externally (dired-copy-filename-as-kill))
      (browse-url (expand-file-name default-directory))))

(map! :leader :desc "Browse or open externally" "o x" #'open-file-externally)

(remove-hook! 'dired-mode-hook #'dired-omit-mode)

(setq company-idle-delay nil)

(add-hook 'company-mode-hook 'company-box-mode)

(defun fd-switch-dictionary()
 (interactive)
 (let* ((dic ispell-current-dictionary)
        (change (if (string= dic "italian") "english" "italian")))
  (ispell-change-dictionary change)
  (message "Dictionary switched from %s to %s" dic change)))

(map! :leader :desc "Switch dictionary" "t d" #'fd-switch-dictionary)

(setq gts-translate-list '(("it" "en")
                           ("en" "it")
                           ("it" "es")
                           ("es" "it")))

(after! go-translate
(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-bing-engine) (gts-google-engine))
       :render (gts-buffer-render))))

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-M-i") nil))
(global-set-key (kbd "<M-tab>") 'complete-symbol)
(define-key evil-normal-state-map (kbd "g .") 'flyspell-auto-correct-word)

(map! :leader :desc "toggle font mode" "t v" #'mixed-pitch-mode)
(map! :leader :desc "Toggle emphasis markers" "t e" #'+org-pretty-mode)
(map! :leader :desc "Toggle emphasis headings" "t h" #'org-tree-slide-heading-emphasis-toggle)
(map! :leader :desc "Toggle centered window" "t C" #'centered-window-mode)

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<C-M-return>") #'org-insert-heading))

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook '(lambda () (text-scale-increase +1)))
(add-hook 'org-mode-hook '(lambda () (modify-syntax-entry ?\' " ")))

(defun robert/org-tree-slide-play-mode-hook ()
  ;; (interactive)
        (+org-pretty-mode)
        (setq display-line-numbers nil))

(defun robert/org-tree-slide-stop-mode-hook ()
  ;; (interactive)
        (+org-pretty-mode)
        (setq display-line-numbers t))

(add-hook 'org-tree-slide-play-hook 'robert/org-tree-slide-play-mode-hook)
(add-hook 'org-tree-slide-stop-hook 'robert/org-tree-slide-stop-mode-hook)

(after! org-tree-slide
  (setq org-tree-slide-cursor-init nil)
  (advice-remove 'org-tree-slide--display-tree-with-narrow
                 #'+org-present--hide-first-heading-maybe-a)
  (remove-hook 'org-tree-slide-mode-hook #'+org-present-prettify-slide-h)
  (add-hook 'org-tree-slide-mode-hook #'+org-present-prettify-slide-h-custom))

(defun +org-present-prettify-slide-h-custom ()
  "Set up the org window for presentation."
  (setq +org-present-text-scale 3)
  (let ((arg (if org-tree-slide-mode +1 -1)))
    (if (not org-tree-slide-mode)
        (when +org-present--last-wconf
          (set-window-configuration +org-present--last-wconf))
      (setq +org-present--last-wconf (current-window-configuration))
      (doom/window-maximize-buffer))
    ;; (when (fboundp 'centered-window-mode)
    ;;   (setq-local cwm-use-vertical-padding t)
    ;;   (setq-local cwm-frame-internal-border 100)
    ;;   (setq-local cwm-left-fringe-ratio -10)
    ;;   (setq-local cwm-centered-window-width 300)
    ;;   (centered-window-mode arg))
    ;; (hide-mode-line-mode arg)
    (+org-pretty-mode arg)
    (cond (org-tree-slide-mode
           (set-window-fringes nil 0 0)
           (when (bound-and-true-p flyspell-mode)
             (flyspell-mode -1))
           (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                     nil 'local)
           (text-scale-set +org-present-text-scale)
           (ignore-errors (org-latex-preview '(4))))
          (t
           (text-scale-set 0)
           (set-window-fringes nil fringe-mode fringe-mode)
           (org-clear-latex-preview)
           (org-remove-inline-images)
           (org-mode)))
    (redraw-display)))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" plain (file+headline "~/Documenti/emacs/org/capture/task.org" "TODO")
           "- [ ] %?"
           :unnarrowed nil)
          ("j" "Journal" entry (file+datetree "~/Documenti/emacs/org/capture/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a\n\n"
           :unnarrowed nil)
          ("n" "Nota" plain (file "~/Documenti/emacs/org/capture/note.org" )
           "* %?\n  %i\n  %a\n\n"
           :unnarrowed nil))))

(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%A, %Y_%m_%d"
      org-journal-file-format "%Y_%m_%d.org")

(map! :leader :desc "Org journal new entry" "J" #'org-journal-new-entry)

(setq org-roam-directory "~/Documenti/emacs/org/roam")

(setq org-roam-capture-templates
      '(("d" "default"
         plain "%?"
         :if-new (file+head "%<%Y_%m_%d_%H%m%s>_${slug}.org" "#+title: ${title}
#+filetags:
#+category: ${title}
#+date: %U\n")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default"
         entry "* %<%H:%M> %?"
         :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default"
;;          entry "* %<%H:%M> %?"
;;          :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(map! :leader :desc "Node insert immediate" "n r i" #'org-roam-node-insert-immediate)
(define-key evil-insert-state-map (kbd "C-M-n") 'org-roam-node-insert-immediate)

(map! :leader :desc "Node insert" "n r I" #'org-roam-node-insert)

;; (defun robert/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     member tag-name (org-roam-node-tags node)))

;; (defun robert/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (robert/org-roam-filter-by-tag name)
;;            (org-roam-node-list))))
