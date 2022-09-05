;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "R.P."
      user-mail-address "")

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
(setq doom-font (font-spec :family "Blex Mono Nerd Font" :size 17) ;; :weight 'regular)
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

(setq evil-search-wrap 'nil)

(setq select-enable-clipboard nil)

(add-to-list '+lookup-provider-url-alist '("Startpage" "https://www.startpage.com/sp/search?query=%s"))

(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)

(set-file-template! "/__\\.py$g" :trigger "__" :mode 'python-mode)

(setq dired-omit-files "^\\...+$")

(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map
     (kbd ")") 'dired-omit-mode))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files")

(setq +workspaces-main "#1")

(defun robert/execute-in-shell-and-put-in-buffer (b e)
  "Run current line as shell code and insert/update output."
  (interactive (list (line-beginning-position)
                     (line-end-position)))
  (save-excursion
    ;; delete old output
    (delete-region
     (progn (forward-line) (point))
     (progn (while (get-text-property (point) '$$)
              (forward-line))
            (point)))

    (unless (bolp) (insert "\n"))
    (let* ((command (buffer-substring-no-properties b e))
           (output (with-temp-buffer
                     (shell-command command t t)
                     (buffer-string)))
           (start (point)))
      (insert (propertize output '$$ t 'rear-nonsticky t))
      (pulse-momentary-highlight-region start (point)))))

(define-key evil-normal-state-map (kbd "Q") 'robert/execute-in-shell-and-put-in-buffer)

(defun robert/kill-current-buffer ()
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
(map! :leader :desc "Kill buffer" "b k" #'robert/kill-current-buffer)
(map! :leader :desc "Kill buffer" "b d" #'kill-buffer-and-window)

(defun open-file-externally ()
  "Open the current file's directory in external file browser."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (consult-file-externally (dired-get-filename))
      (browse-url (expand-file-name default-directory))))

(map! :leader :desc "Browse or open externally" "o x" #'open-file-externally)

;; (remove-hook! 'dired-mode-hook #'dired-omit-mode)

(defun robert/dired-popup-this-location ()
  "Open popup dired buffer of current file"
  (interactive)
  (dired-other-window default-directory))

(map! :leader :desc "Explore this dir" "x" #'robert/dired-popup-this-location)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package dwim-shell-command
  :commands (dwim-shell-command dwim-shell-command-on-marked-files))

(defun robert/dwim-shell-command-add-pages-to-pdf ()
  "Add the page numbers to a pdf file"
  (interactive)
  (dwim-shell-command-on-marked-files
  "Add the page numbers to a pdf file"
"
enscript --fancy-header=footer --header-font='Times-Roman11' \
-L1 --header='' --footer='|$%|' -o- < <(for i in $(seq 1 400); do echo; \
done) | ps2pdf - | pdftk '<<f>>' multistamp - output '<<fne>>_numbered.pdf'
"
   :utils '("enscript" "pdftk" "ps2pdf" "seq")
   :extensions "pdf"))

(defun robert/dwim-shell-command-mark-pdf-with-file-name ()
  "Add pdf name in header of file"
  (interactive)
  (let ((filename (file-name-base (dired-get-filename))))
    (dwim-shell-command-on-marked-files
     "Add pdf name in header of file"
     (format " enscript --fancy-header=footer --header-font='Times-Roman11' -L1 --header=''%s'||' --footer='' -o- < <(for i in $(seq 1 400); do echo; done) | ps2pdf - | pdftk '<<f>>' multistamp - output '<<fne>>_marked.pdf'" 
             filename)))
  :utils '("enscript" "pdftk" "ps2pdf" "seq")
  :extensions "pdf"
  :silent-success)

(map! :leader "SPC" nil)

(map! :leader "X" nil)

(map! :leader "t [" #'smartparens-mode)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(global-set-key (kbd "<menu>") 'save-buffer)

(define-key evil-insert-state-map (kbd "\C-e") 'evil-copy-from-below)

(global-set-key (kbd "S-<insert>") 'clipboard-yank)
(define-key evil-visual-state-map (kbd "C-<insert>") 'robert/copy)
(define-key evil-visual-state-map (kbd "S-<deltechar>") 'clipboard-kill-region)

(defun robert/copy ()
  "Copy to system clipboard"
  (interactive)
  (evil-use-register ?+)
  (call-interactively 'evil-yank))
(global-set-key (kbd "C-<insert>") 'robert/copy)

(map! :leader :desc "Grep" "/" #'grep)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq fancy-splash-image "~/Pictures/.emacs_mars.png")

(assoc-delete-all "Reload last session" +doom-dashboard-menu-sections)
(assoc-delete-all "Open org-agenda" +doom-dashboard-menu-sections)
(assoc-delete-all "Open project" +doom-dashboard-menu-sections)
(assoc-delete-all "Open documentation" +doom-dashboard-menu-sections)

(remove-hook! '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "I showed you my config files, pls respond")))

(setq all-the-icons-scale-factor 1.0)

(add-hook 'after-init-hook #'display-battery-mode)
(add-hook 'after-init-hook #'display-time)
;; (add-hook 'after-init-hook #'menu-bar-mode)
(setq 
 display-time-format "%a·%d/%m/%y·%H:%M"
 ;; display-time-24hr-format t
 ;; display-time-day-and-date t
 display-time-default-load-average 3)

(set-popup-rules!
  '(
    ("*Async Shell Command*"
     :side bottom
     :size 0.30
     :quit t
     :select nil)
    )
  )

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

(setq company-idle-delay nil)

(add-hook 'company-mode-hook 'company-box-mode)

(after! ispell
  (ispell-change-dictionary "italian"))

(defun fd-switch-dictionary()
 (interactive)
 (let* ((dic ispell-current-dictionary)
        (change (if (string= dic "italian") "english" "italian")))
  (ispell-change-dictionary change)
  (message "Dictionary switched from %s to %s" dic change)))

(map! :leader :desc "Switch dictionary" "t d" #'fd-switch-dictionary)

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-M-i") nil))
(global-set-key (kbd "<M-tab>") 'complete-symbol)
(define-key evil-normal-state-map (kbd "g .") 'flyspell-auto-correct-word)

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook '(lambda () (text-scale-increase +1)))
(add-hook 'org-mode-hook '(lambda () (modify-syntax-entry ?\' " ")))

(setq org-ellipses " ^ ")

(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
        (set-buffer (get-buffer "*Occur*"))
        (goto-char (point-min))
        (read-only-mode 0)
        ;; (if (looking-at "^[0-9]+ lines matching \"")
        ;;     (kill-line 1))
        ;; (flush-lines "^[0-9]+ matches for")
        (while (re-search-forward "^[ \t]*[0-9]+:"
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1))
        (+evil/window-move-left) 
        (evil-window-increase-width 28)
        ;; (+popup-mode)
        (hide-mode-line-mode)
        (+word-wrap-mode)
        (text-scale-adjust -1)
        ;; (rename-buffer (concat "*" buff-name "-Occur*"))
        (occur-rename-buffer nil t)
        (read-only-mode 1))
    (message "There is no buffer named \"*Occur*\".")))
;; (add-hook 'occur-hook #'occur-mode-clean-buffer)

(defun robert/occur-tree-org ()
  "Show headings of org file"
  (interactive)
  (occur "^\*+ ")
  (occur-mode-clean-buffer))

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Show Org tree" ";" #'robert/occur-tree-org)

(defun aj-fetch-latest (path)
  (let ((e (f-entries path)))
    (car (sort e (lambda (a b)
                   (not (time-less-p (aj-mtime a)
                                     (aj-mtime b))))))))
(defun aj-mtime (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))

(defun insert-org-image--time-dependent ()
  "Moves image from screenshot folder to `buffer-file-name'_media, inserting org-mode link"
  (interactive)
  (let* (
         ;; (indir (expand-file-name ~/Documenti/emacs/screenshots))
         (infile (aj-fetch-latest "~/Documenti/emacs/screenshots"))
         ;; (infile (get-newest-file-from-dir "~/Documenti/emacs/screenshots"))
         (outdir (concat (buffer-file-name) "_media"))
         (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (when (or
           (string-equal "0" (format-time-string "%-M" 
                                                 (time-since (f-modification-time infile))))
           (string-equal "1" (format-time-string "%-M" 
                                                 (time-since (f-modification-time infile)))))
      (rename-file infile outfile)
      (insert (concat (concat 
                       "[[./" 
                       (file-name-nondirectory (buffer-file-name)) 
                       "_media/" 
                       (file-name-nondirectory outfile)) 
                      "]]"))))
  (newline)
  (newline))

(defun insert-org-image--time-independent ()
  "Moves image from screenshot folder to `buffer-file-name'_media, inserting org-mode link"
  (interactive)
  (let* (
         ;; (indir (expand-file-name ~/Documenti/emacs/screenshots))
         (infile (aj-fetch-latest "~/Documenti/emacs/screenshots"))
         ;; (infile (get-newest-file-from-dir "~/Documenti/emacs/screenshots"))
         (outdir (concat (buffer-file-name) "_media"))
         (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (rename-file infile outfile)
    (insert (concat (concat 
                     "[[./" 
                     (file-name-nondirectory (buffer-file-name)) 
                     "_media/" 
                     (file-name-nondirectory outfile)) 
                    "]]")))
  (newline)
  (newline))

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert screenshot (last 1m)" "a i" #'insert-org-image--time-dependent)
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert screenshot" "a I" #'insert-org-image--time-independent)
;; (map! :after org
;;       :map org-mode-map
;;       :localleader
;;       :desc "Insert screenshot" "<print>" #'insert-org-image--time-dependent)

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Toggle font style" "F" #'mixed-pitch-mode)

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Pretty-mode toggle" "P" #'+org-pretty-mode)

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<C-M-return>") #'org-insert-heading))

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
  (setq +org-present-text-scale 5)
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

(map! :leader :desc "Org-J new entry" "J" #'org-journal-new-entry)

(setq org-roam-directory "~/Documenti/emacs/org/roam")

(setq org-roam-capture-templates
      '(("d" "default"
         plain "%?"
         :if-new (file+head "${slug}_%<%Y_%m_%d_%H%m%s>.org" "#+title: ${title}
#+filetags:
#+category: ${title}
#+date: %U\n")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default"
         entry "* %<%H:%M> %?"
         :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;;; IDK
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
