;; package --- My init.el
;;; Commentary:
;;; Code:

(load "~/.emacs.d/helpers")
(load "~/.emacs.d/pkgs/tssh-tramp.el")

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
 ;; resize-mini-windows nil
 )
(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace nil)
;; (add-to-list 'default-frame-alist '(font . "Fira Code 12"))
(add-to-list 'default-frame-alist '(font . "Victor Mono 14"))
(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
(blink-cursor-mode -1)

(load-file "~/.emacs.d/pkgs/russian-mac.el")
(setq default-input-method "russian-mac")

(mac-auto-operator-composition-mode)  ; ligatures support

;; macbook keyboard modifications
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control   ; left-control
        mac-function-modifier 'control  ; left-control
        mac-option-modifier 'control    ; right-control
        mac-command-modifier 'meta
        mac-pass-command-to-system nil))

;; common modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)
;(key-chord-mode 1)
(recentf-mode 1)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)
(setq bidi-inhibit-bpa t)

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

;; buffer moving
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; enable some commands
(put 'erase-buffer 'disabled nil)

;; auto-delete trailing whitespace
(add-hook 'write-file-hooks
          (lambda ()
            (when (not (derived-mode-p 'markdown-mode))  ; trailing whitespaces are meaningful in markdown
              (delete-trailing-whitespace)
              )))

;; prefer ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; helm mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

(setq helm-split-window-inside-p t)
(when (executable-find "curl")
  (setq helm-net-prefer-curl t))
(helm-mode 1)

;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(setq helm-swoop-split-with-multiple-windows t)

;; helm-ag
(global-set-key (kbd "C-c g") 'project/ag)
;; rg --vimgrep --no-heading --smart-case
;; ag --hidden --nocolor --nogroup --ignore-case --ignore=.git --ignore=.svn
(setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case --hidden --glob !.git"
      helm-ag-insert-at-point 'symbol)

;; helm-ls-git
(require 'helm-ls-git)
(global-set-key (kbd "C-c f") 'helm-browse-project)
(global-set-key (kbd "C-x r p") 'helm-projects-history)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
            ))

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-c C-m") 'mc/mark-all-in-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-c C-SPC") 'set-rectangular-region-anchor)

;; fast cursor move
(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(setq avy-all-windows nil) ; this window only

;; text selection
(require 'expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

;; text moving
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

;; handy pairs
;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-{") 'insert-pair)
;; (global-set-key (kbd "M-\"") 'insert-pair)
;; (global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; tree-sitter
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
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)))

;; combobulate
(add-to-list 'load-path "~/.emacs.d/pkgs/combobulate")
(load "combobulate.el")
(eval-after-load "combobulate"
  '(progn
     (define-key combobulate-key-map [M-left] nil)
     (define-key combobulate-key-map [M-right] nil)
     (define-key combobulate-key-map [M-up] nil)
     (define-key combobulate-key-map [M-down] nil)
     (define-key combobulate-key-map [C-left] 'combobulate-navigate-logical-previous)
     (define-key combobulate-key-map [C-right] 'combobulate-navigate-logical-next)
     (define-key combobulate-key-map [C-up] 'combobulate-navigate-up-list-maybe)
     (define-key combobulate-key-map [C-down] 'combobulate-navigate-down-list-maybe)
     ))

;; direnv
(direnv-mode)

;; spell checking
(global-set-key (kbd "C-c s") 'isharov/toggle-flyspell)

;; flymake
(global-set-key (kbd "C-c e") 'flymake-show-diagnostics-buffer)

;; tramp mode
(setq password-cache-expiry nil)

;; docker
(global-set-key (kbd "C-c d") 'docker)

;; k8s
(require 'kubel)
(kubel-vterm-setup)
(setq kubel-log-tail-n 1000)

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

;; js
;; npm install -g eslint eslint-plugin-react
;; /usr/local/bin/eslint -> /usr/local/lib/node_modules/eslint/bin/eslint.js --resolve-plugins-relative-to=/usr/local/lib/node_modules/ $@
(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "M-,") 'buffer/tag-region)
            (setq-default sgml-basic-offset 4)
            (setq indent-tabs-mode nil)
            ))

;; C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-x t") 'isharov/toggle-source)
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; python
;; M-x find-library RET python RET
;; (setq python-ts-mode-hook python-mode-hook)
(add-hook 'python-ts-mode-hook
          (lambda ()
            (eglot-ensure)
            (combobulate-mode)
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            (local-set-key (kbd "C-c C-f")
                           (lambda ()
                             (interactive)
                             (buffer/shell-command "black")
                             (buffer/shell-command "isort")
                             ))
            (local-set-key (kbd "C-c C-s") (lambda () (interactive) (buffer/shell-command "isort")))
            ))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-ts-mode)
              (flymake-ruff-load))
            ))
;; (add-hook 'inferior-python-mode-hook
;;           (lambda ()
;;             (comint/turn-on-history)
;;             (define-key inferior-python-mode-map (kbd "M-r") 'helm-comint-input-ring)
;;             ))

;; go
;; go install golang.org/x/tools/gopls@latest
; (add-hook 'go-mode-hook #'lsp-deferred)

;; rust
;; rustup component add rust-analyzer
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (eglot-ensure)
            (combobulate-mode)
            ))

;; git
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk t)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
; (diff-hl-margin-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq diff-hl-disable-on-remote t)

;; svn
;; (require 'dsvn)
;; (global-set-key (kbd "C-c v s") 'isharov/svn-status)

;; color-theme
(when (window-system)
  (load-theme 'zenburn t)
  (set-face-italic 'font-lock-comment-face 1)
  )

;; shell
(add-to-list 'load-path "~/.emacs.d/pkgs/helm-comint/")
(load "helm-comint.el") ;; TODO: is it deprecated? What's the alternative
(add-hook 'shell-mode-hook 'comint/turn-on-history)
;(add-hook 'shell-mode-hook 'buffer-disable-undo)
;(add-hook 'shell-mode-hook (lambda () (goto-address-mode)))
;(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint/write-input-ring-all-buffers)
(define-key shell-mode-map (kbd "M-r") 'helm-comint-input-ring)
(setq
 comint-input-ignoredups t           ; no duplicates in command history
 ;comint-completion-addsuffix t      ; insert space/slash after file completion
 comint-get-old-input (lambda () "") ; what to run when i press enter on a line above the current prompt
 comint-input-ring-size 5000         ; max shell history size
)
; company completion would stuck on slow tramp connection
(add-hook 'shell-mode-hook
          (lambda ()
            (if (file-remote-p (path/current-dir))
                (company-mode -1))))

(defun shell-acamar ()
  "Shortcut for acamar remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@acamar#20002:/"))
    (shell "*shell-acamar*")))

(defun shell-bastion ()
  "Shortcut for bastion remote shell."
  (interactive)
  (let ((default-directory "/tssh:isharov@bastion:/home/isharov/"))
    (shell "*shell-bastion*")))

(defun shell-gitlab ()
  "Shortcut for gitlab remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@gitlab:/root/"))
    (shell "*shell-gitlab*")))

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
(require 'restclient)
(setq restclient-inhibit-cookies t)  ;; enforce explicit cookies
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

(let ((default-directory (or (getenv "EMACS_DEFAULT_DIRECTORY") "~/dev")))
  (magit-status)
  (shell "*shell*")
  (shell "*shell*<1>")
  )
(execute-kbd-macro "\C-m")
(buf-move-left)
(toggle-frame-fullscreen)

(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
     When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil 0 nil file))
  )

(provide 'init)
;;; init.el ends here
