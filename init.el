;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load "~/.emacs.d/helpers")
(load "~/.emacs.d/pkgs/tssh-tramp.el")
;; must load before `savehist-mode' below, so savehist can restore into it
(load "~/.emacs.d/pkgs/global-history.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; (dolist (pkgdir (path/subdirs "~/.emacs.d/pkgs"))
;;   (add-to-list 'load-path pkgdir))

;; common editor customization
(setq
 c-default-style "linux"
 c-basic-offset 4
 require-final-newline t
 kill-whole-line t
 save-interprogram-paste-before-kill t  ;; Save the Clipboard Before Killing
 kill-do-not-save-duplicates t
 recentf-max-saved-items 5000
 enable-recursive-minibuffers t
 history-delete-duplicates t
 history-length 100
 scroll-preserve-screen-position 'always
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil
 ring-bell-function 'ignore
 max-mini-window-height 4
 window-combination-resize t  ;; Proportional Window Resizing
 ;; resize-mini-windows nil
 )
(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace nil)

(setopt sentence-end-double-space nil)  ; archaic default
(setopt view-lossage-auto-refresh t)    ; live-updating C-h l
(setopt project-mode-line t)            ; project name in the mode line
;; C-o is `isharov/find-file-at-point'; without this ffap will DNS-ping
;; anything at point that merely looks like a hostname.
(setopt ffap-machine-p-known 'reject)

(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w"))) ; '_' is part of a word in all modes

(setopt use-short-answers t) ; type y/n instead of yes/no
(blink-cursor-mode -1)

(load-file "~/.emacs.d/pkgs/russian-mac.el")
(setq default-input-method "russian-mac")

;; macbook keyboard modifications
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control   ; left-control
        mac-function-modifier 'control  ; left-control
        mac-option-modifier 'control    ; right-control
        mac-command-modifier 'meta
        mac-pass-command-to-system nil)
  ;; the click that focuses Emacs only raises the window, it does not move point
  (setopt ns-click-through nil))

;; common modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
;; Use the OS file-notification interface instead of stat'ing every buffer
;; every `auto-revert-interval' seconds -- that polling is costly with TRAMP
;; buffers open.  Set back to nil if file changes stop being picked up.
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)
(setopt show-paren-delay 0)
(setopt show-paren-context-when-offscreen 'overlay) ; show an offscreen openparen
(show-paren-mode 1)
(electric-pair-mode 1)
;(key-chord-mode 1)
;; Never recall files from temp dirs -- scratch/agent working directories under
;; /tmp would otherwise flood the list (they are real files, so recentf-cleanup
;; will not drop them).
(setq recentf-exclude '("\\`/tmp/" "\\`/private/tmp/" "\\`/var/folders/"))
(recentf-mode 1)
(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))
(global-hl-line-mode 1)
(which-key-mode -1)    ; no popup of follow-up keys after a prefix
(global-eldoc-mode -1) ; no docs of the symbol at point in the echo area
(repeat-mode 1)        ; repeatable key sequences without re-pressing the prefix
(when (display-graphic-p)
  (mouse-shift-adjust-mode 1)) ; shift-drag adjusts the region instead of restarting it

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)
;; These settings tell Emacs to assume left-to-right text everywhere
;; and skip the bidirectional parenthesis algorithm
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;; Skip Fontification During Input
(setq redisplay-skip-fontification-on-input t)
;; Increase Process Output Buffer for LSP
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

;; Always Open Emacs Buffers in the Current Active Window
(use-package single-window
  :vc (:url "https://github.com/jamescherti/single-window.el"
       :rev :newest)
  :config
  (single-window-mode 1)
  ;; hide the " single-window" mode-line lighter
  (setcdr (assq 'single-window-mode minor-mode-alist) '("")))

;; buffer moving
(use-package buffer-move
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

;; enable some commands
(put 'erase-buffer 'disabled nil)

;; auto-delete trailing whitespace
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'markdown-mode))  ; trailing whitespaces are meaningful in markdown
              (delete-trailing-whitespace)
              )))

;; prefer ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; minibuffer completion stack (vertico + orderless + marginalia + consult + embark)
(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :custom (vertico-cycle t))

;; helm-find-files-like path navigation: RET/TAB descend, DEL goes up a component
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

;; hide M-x commands irrelevant to the current major mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; restore default bindings that helm had overridden (vertico now drives them)
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-o") #'isharov/find-file-at-point)  ;; was open-line
;; (global-set-key (kbd "C-c o") #'open-line)  ;; open-line moved off C-o

;; consult: enhanced search / navigation commands
(use-package consult
  :ensure t
  :bind (("M-y"     . consult-yank-pop)      ;; was helm-show-kill-ring
         ("C-x b"   . consult-buffer)        ;; was helm-mini
         ("C-x C-r" . consult-recent-file)   ;; was helm-recentf
         ("C-x r b" . consult-bookmark)      ;; was helm-filtered-bookmarks
         ("M-i"     . isharov/consult-line)  ;; was helm-swoop (seed with selection)
         ("M-I"     . consult-line-multi)    ;; multi-buffer swoop
         ("M-g i"   . consult-imenu)         ;; jump to symbol/heading in buffer
         ("M-g I"   . consult-imenu-multi))  ;; ...across project buffers
  :custom (consult-line-start-from-top t))

;; consult-dir: jump the minibuffer to recent dirs, bookmarks, and TRAMP hosts
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)                 ;; was list-directory (rarely used)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))     ;; minibuffer-only; global C-x C-j stays dired-jump

;; project / directory ripgrep search (was helm-ag; project/ag lives in helpers.el)
(global-set-key (kbd "C-c g") 'project/ag)                      ;; project search
(global-set-key (kbd "C-c G")                                  ;; custom dir search (prompts for dir)
                (lambda () (interactive)
                  (consult-ripgrep '(4) (isharov/selection))))

;; project file finding / switching (was helm-ls-git; project-wide, no git requirement)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c p") 'project-switch-project)         ;; short alias for C-x p p

;; embark: contextual actions + export results to an editable buffer
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; which-key is off (see above); this brings back the *automatic* popup of
  ;; follow-up keys after a prefix, rendered through embark's completing-read.
  (setopt embark-auto-prefix-help-delay 1.0)
  (embark-auto-prefix-help-mode 1))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; in-buffer completion: corfu + cape (was company)
(use-package corfu
  :ensure t
  :init (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(global-set-key (kbd "C-<tab>") #'completion-at-point)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(put 'dired-find-alternate-file 'disabled nil)
;; (setq dired-listing-switches "-la")
(add-hook 'dired-mode-hook
          (lambda ()
            ;; (dired-omit-mode)
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
            ))

;; multiple cursors
;; (every command below is autoloaded by the package, so :bind defers the load
;;  until the first cursor is actually marked)
(use-package multiple-cursors
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-S-c C-<"   . mc/mark-all-like-this)
         ("C-S-c C->"   . mc/mark-more-like-this-extended)
         ("C-S-c C-m"   . mc/mark-all-in-region)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-S-c C-SPC" . set-rectangular-region-anchor)))

;; fast cursor move
(use-package flash
  :commands (flash-jump flash-jump-continue flash-treesitter)
  :bind ("C-'" . flash-jump)
  :custom
  (flash-multi-window t)
  :init
  ;; Search integration (labels during C-s, /, ?)
  ;; (require 'flash-isearch)
  ;; (flash-isearch-mode 1)
  )

;; isearch
;; (`isearch-forward-thing-at-point' is already on the global M-s M-., and
;;  M-s . is isearch-forward-symbol-at-point -- C-. stays embark-act.)
(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)                    ; "(3/17)" match counter in the prompt
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-allow-motion t)                  ; C-a/C-e/M-</M-> move between matches
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)    ; C-r goes straight to the previous match
  (isearch-wrap-pause 'no-ding))

;; text selection
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

(use-package expreg
  :ensure t
  :bind (("C-M-SPC" . expreg-expand)
         ("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package treesit-sexp
  :vc (:url "https://github.com/alexispurslane/treesit-sexp"
       :rev :newest)
  :config
  (global-treesit-sexp-mode 1))

;; text moving
(use-package move-text
  :bind (("<M-S-up>"   . move-text-up)
         ("<M-S-down>" . move-text-down)))

;; handy pairs
;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-{") 'insert-pair)
;; (global-set-key (kbd "M-\"") 'insert-pair)
;; (global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; undo
;; (global-set-key (kbd "C-/") 'vundo)

;; project extra markers
(setq project-vc-extra-root-markers '(".project"))

;; project-switch-project (C-x p p, C-c p) dispatch menu.  The stock entries
;; (find-regexp / find-dir / vc-dir / eshell / any-command) are replaced -- we
;; use ripgrep, magit and ghostel instead.  The custom commands live in
;; helpers.el; they resolve the root themselves because project-switch-project
;; only binds project-current-directory-override, never default-directory.
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "m") 'project/magit-status)
  (define-key project-prefix-map (kbd "s") 'project/ghostel)  ;; was project-shell
  (setq project-switch-commands
        '((project-find-file    "Find file" ?f)
          (project/ag           "Search"    ?g)
          (project-dired        "Dired"     ?d)
          (project/magit-status "Magit"     ?m)
          (project/ghostel      "Shell"     ?s))))

;; tree-sitter
;; Where to fetch each grammar.  `treesit-auto-install-grammar' below makes
;; Emacs offer to install one the first time a mode needs it.
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        ;; js-ts-mode needs jsdoc alongside javascript
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")))
(setopt treesit-auto-install-grammar 'ask)  ; installs into ~/.emacs.d/tree-sitter

;; `treesit-enabled-modes' (new in Emacs 31) replaces the hand-written
;; major-mode-remap-alist that used to live here.  It is lazy -- the ts-mode is
;; picked when a file is actually visited -- so unlike treesit-auto there is no
;; global minor mode scanning every file-open (incl. consult previews).
;;
;; This is an explicit list rather than t on purpose.  t enables *every*
;; ts-mode, including c-ts-mode, c++-ts-mode, java-ts-mode and ruby-ts-mode,
;; for which we have no grammars: visiting a .c or .java file then signals a
;; mode error and leaves the buffer in a ts-mode with no parser at all -- no
;; font-lock, no indentation.  It would also route C/C++ away from the cc-mode
;; settings configured at the top of this file.  markdown-ts-mode is left out
;; for a different reason: it derives from text-mode, not markdown-mode, so the
;; trailing-whitespace hook above would stop exempting markdown files.
(setopt treesit-enabled-modes
        '(bash-ts-mode        ; NB: the old list said bash-mode, which does not
                              ; exist (it is sh-mode), so this never fired
          cmake-ts-mode
          css-ts-mode
          dockerfile-ts-mode
          go-ts-mode
          mhtml-ts-mode      ; .html; html-ts-mode is not an accepted value here
          js-ts-mode
          json-ts-mode
          python-ts-mode
          rust-ts-mode
          toml-ts-mode
          tsx-ts-mode
          typescript-ts-mode
          yaml-ts-mode))

;; direnv
(use-package direnv
  :demand t                ; the mode has to be live before the first file opens
  :config (direnv-mode))

;; flymake
(global-set-key (kbd "C-c e") 'consult-flymake) ;; navigable diagnostics list (was flymake-show-buffer-diagnostics)

;; tramp mode
(setq password-cache-expiry nil)
(setq tramp-histfile-override t)  ;; disable history file

;; docker
(use-package docker
  :bind ("C-c d" . docker))

;; k8s
(use-package kubel
  ;; `kubel-set-kubectl-config-file' is NOT in kubel's own autoloads, and the
  ;; k8s/* helpers below call it *before* the autoloaded `kubel-open' -- listing
  ;; it here makes use-package generate the autoload, so the helpers pull kubel
  ;; in on first use.
  :commands (kubel kubel-open kubel-set-kubectl-config-file)
  :custom (kubel-log-tail-n 1000))

(defun k8s/zent-staging ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/zent.staging.config")
  (kubel-open "zent-staging" "staging" "pods"))

(defun k8s/zent-prod ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/zent.prod.config")
  (kubel-open "zent-production" "production" "pods"))

(defun k8s/clapp-staging ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/clapp.staging.config")
  (kubel-open "clapp-staging" "staging" "pods"))

;; (require 'kubed)
;; (keymap-global-set "C-c k" 'kubed-prefix-map)
;; (keymap-set kubed-prefix-map "k" #'kubed-transient)

;; gptel
;; (setq
;;  gptel-model 'phi4:latest
;;  gptel-backend (gptel-make-ollama "Ollama"
;;                  :host "localhost:11434"
;;                  :stream t
;;                  :models '(phi4:latest qwen2.5-coder:14b)))

;; aider
;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   :custom
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-show-diff-after-change nil)
;;   (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4")
;;   (aidermacs-architect-model "openrouter/anthropic/claude-opus-4")
;;   )

;; eglot
(use-package eglot
  :ensure nil  ; built in since Emacs 29
  :config (add-to-list 'eglot-server-programs
                       '((python-mode python-ts-mode) "basedpyright-langserver" "--stdio"))
  ;; :config (add-to-list 'eglot-server-programs
  ;;                      '((python-mode python-ts-mode) "ty" "server"))
  (setq eglot-report-progress nil)
  ;; Every LSP event is otherwise appended to a per-server event buffer, which
  ;; is the single biggest eglot cost on a chatty server.
  (fset #'jsonrpc--log-event #'ignore)
  (setopt eglot-send-changes-idle-time 0.1)
  (setopt eglot-extend-to-xref t)  ; manage files outside the project reached via xref
  )

;; copilot
;; it has implicit editorconfig melpa dependency
;; (add-to-list 'load-path "~/.emacs.d/pkgs/copilot.el")
;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-mode-map (kbd "M-C-<return>") 'copilot-accept-completion)

;; scala
;(require 'scala-mode2)
;(add-hook 'scala-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c C-r")
;                           (lambda()
;                             (interactive)
;                             (buffer/create-send-region "*sbt-console*" "sbt console-quick")))))

;; xml
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; Javascript / JSON
;; npm install -g eslint eslint-plugin-react
;; /usr/local/bin/eslint -> /usr/local/lib/node_modules/eslint/bin/eslint.js --resolve-plugins-relative-to=/usr/local/lib/node_modules/ $@
(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "M-,") 'buffer/tag-region)
            (setq-default sgml-basic-offset 4)
            (setq indent-tabs-mode nil)
            ))
(setq json-ts-mode-indent-offset 4)

;; C++
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-t") 'isharov/toggle-source))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; python
;; M-x find-library RET python RET
(with-eval-after-load 'python
  (define-key python-ts-mode-map (kbd "C-c C-f")   ;; was python-eldoc-at-point
              (lambda ()
                (interactive)
                (buffer/shell-command "ruff format")
                (buffer/shell-command "ruff check --fix --unsafe-fixes")
                (revert-buffer t t t)
                )))
(add-hook 'python-ts-mode-hook
          (lambda ()
            ;; delete region if active else dedent
            (local-set-key (kbd "<backspace>")
                           (lambda ()
                             (interactive)
                             (if (use-region-p)
                                 (delete-backward-char 1)
                               (python-indent-dedent-line-backspace 1))
                             ))
            ))
(use-package flymake-ruff
  :commands (flymake-ruff-load)
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-ts-mode)
                (flymake-ruff-load)))))
;; (add-hook 'inferior-python-mode-hook
;;           (lambda ()
;;             (comint/turn-on-history)
;;             (define-key inferior-python-mode-map (kbd "M-r") 'consult-history)
;;             ))

;; go
;; go install golang.org/x/tools/gopls@latest
(add-hook 'go-mode-hook 'eglot-ensure)

;; rust
;; rustup component add rust-analyzer

;; git
(use-package magit
  :bind (("C-x g" . magit-status)
         :map magit-mode-map
         ("C-o" . magit-diff-visit-worktree-file-other-window))
  :custom
  (magit-diff-refine-hunk 'all)
  ;; single-window-mode forces every buffer into the current window, so the
  ;; commit diff would immediately replace COMMIT_EDITMSG. Skip it; C-c C-d
  ;; (magit-diff-while-committing) shows it on demand.
  (magit-commit-show-diff nil))
;; (helm couldn't do completing-read-multiple, so magit octopus-merge selection
;;  used to be advised down to a single read here; vertico handles CRM natively.)

(use-package diff-hl
  :demand t              ; the gutter should be there from the first buffer on
  :custom (diff-hl-disable-on-remote t)
  ;; NB: only post-refresh is real.  `diff-hl-magit-pre-refresh' has been an
  ;; obsolete alias for `ignore' since diff-hl 1.11.0, so the pre-refresh hook
  ;; this config used to add did nothing; dropped.
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  ;; (diff-hl-margin-mode)
  )

;; svn
;; (require 'dsvn)
;; (global-set-key (kbd "C-c v s") 'isharov/svn-status)

;; color-theme
(use-package doom-themes
  :if (window-system)
  :demand t
  :config (load-theme 'doom-one t))

;; (font setup lives in helpers.el and is independent of the theme package)
(when (window-system)
  (theme/setup-font))

;; shell
(add-hook 'shell-mode-hook 'comint/turn-on-history)
;(add-hook 'shell-mode-hook 'buffer-disable-undo)
;(add-hook 'shell-mode-hook (lambda () (goto-address-mode)))
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint/write-input-ring-all-buffers)
;; consult-history reads comint-input-ring (was helm-comint-input-ring)
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "M-r") 'history/pick))
(setq
 comint-input-ignoredups t           ; no duplicates in command history
 ;comint-completion-addsuffix t      ; insert space/slash after file completion
 comint-get-old-input (lambda () "") ; what to run when i press enter on a line above the current prompt
 comint-input-ring-size 5000         ; max shell history size
)
; in-buffer completion would stuck on slow tramp connection
(add-hook 'shell-mode-hook
          (lambda ()
            (if (file-remote-p (path/current-dir))
                (corfu-mode -1))))
;; ghostel
(use-package ghostel
  :ensure t
  ;; C-r is left to the terminal (fzf), M-r is the Emacs-side history picker
  ;; (this buffer's shell + the global history; C-u M-r for global only).
  :bind (:map ghostel-mode-map
         ("M-r" . history/pick))
  :custom
  ;; keep copy mode after M-w instead of exiting back to semi-char
  ;; (ghostel-readonly-fast-exit nil)
  ;; semi-char mode encodes C-S-<arrow> and sends it to the pty, so the
  ;; global buf-move-* bindings never fire in a terminal buffer.  Listing
  ;; them here leaves them unbound in ghostel-semi-char-mode-map, so they
  ;; fall through to the global map.  The first line is ghostel's default.
  (ghostel-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
     "C-S-<up>" "C-S-<down>" "C-S-<left>" "C-S-<right>"
     ;; M-r: ghostel/history instead of the shell's own binding
     "M-r"))
  ;; scrollback in bytes
  (ghostel-max-scrollback (* 32 1024 1024)))

;; Record every line submitted at a prompt -- ghostel (including shells
;; reached through ssh/docker inside a terminal) and comint alike.
;; See pkgs/global-history.el.
(history/global-setup)

(global-set-key (kbd "C-c s") 'project/ghostel)  ;; new terminal in the project root

;; Remote shells.  These were comint `shell' buffers; ghostel spawns the
;; shell on the far end of the TRAMP path instead, so they are real ptys
;; (fzf, htop, less behave) and feed the global history like any other
;; terminal.  The macro lives in helpers.el.
(ghostel/define-remote-shell arneb    "/ssh:root@arneb#20002:/")
(ghostel/define-remote-shell bastion  "/ssh:isharov@bastion.prd.clapp.clteam.io:/home/isharov/")
(ghostel/define-remote-shell gitlab   "/ssh:root@gitlab:/root/")
(ghostel/define-remote-shell synology "/ssh:192.168.1.3:~/")
(ghostel/define-remote-shell ob-analyzer-finland-01 "/ssh:root@ob-analyzer-finland-01:/root/")
(ghostel/define-remote-shell ob-analyzer-finland-02 "/ssh:root@ob-analyzer-finland-02:/root/")
(ghostel/define-remote-shell ob-analyzer-finland-03 "/ssh:root@ob-analyzer-finland-03:/root/")
(ghostel/define-remote-shell ob-analyzer-finland-04 "/ssh:root@ob-analyzer-finland-04:/root/")
(ghostel/define-remote-shell ob-analyzer-germany    "/ssh:root@ob-analyzer-germany:/root/")
(ghostel/define-remote-shell stg-ob-analyzer-00     "/ssh:root@stg-ob-analyzer-00:/root/")


;; org
(eval-after-load "org"
  '(progn
     (define-key org-mode-map [M-left] nil)
     (define-key org-mode-map [M-right] nil)
     (define-key org-mode-map [M-up] nil)
     (define-key org-mode-map [M-down] nil)
     (define-key org-mode-map [C-left] 'org-metaleft)
     (define-key org-mode-map [C-right] 'org-metaright)
     (define-key org-mode-map [C-up] 'org-metaup)
     (define-key org-mode-map [C-down] 'org-metadown)
     ))

;; mermaid
;; npm install -g @mermaid-js/mermaid-cli
;; (setq mermaid-mmdc-location "docker")
;; (setq mermaid-flags "run --rm -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6")
(setq mermaid-flags "-s 3")
;; (setq mermaid-output-format ".svg")

;; restclient.el
(use-package restclient
  :commands (restclient-mode)
  :custom (restclient-inhibit-cookies t))  ;; enforce explicit cookies
;(setq tls-program '("gnutls-cli --insecure --x509cafile %t -p %p %h" "gnutls-cli --insecure --x509cafile %t -p %p %h --protocols ssl3"))
;(custom-reevaluate-setting 'tls-program)

;; setup default desktop
(setq inhibit-startup-screen t)
(toggle-frame-maximized)
(split-window-vertically)
(split-window-horizontally)
(windmove-down)
(split-window-horizontally)
(windmove-up)
(balance-windows)  ; equal splits even if the frame settled at an odd size

;; (let ((default-directory (or (getenv "EMACS_DEFAULT_DIRECTORY") "~/dev")))
;;   (ghostel 1)
;;   (ghostel 2)
;;   )

(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
     When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil 0 nil file))
  )

;; Restore the GC threshold early-init.el raised for the duration of startup.
;; `bound-and-true-p' because --batch does not load early-init.el at all.
(setq gc-cons-threshold (or (bound-and-true-p bedrock--initial-gc-threshold) 800000))

(provide 'init)
;;; init.el ends here
