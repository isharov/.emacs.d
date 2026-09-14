;;; early-init.el --- Early init  -*- lexical-binding: t; -*-

;; Garbage collection is a large share of startup cost.  Raise the threshold
;; for the duration of init; the end of init.el restores it.
(defvar bedrock--initial-gc-threshold gc-cons-threshold
  "The value of `gc-cons-threshold' before init raised it.")
(setq gc-cons-threshold (* 10 1000 1000))

;; Quiet startup: no byte-compile/native-comp chatter, no startup echo message.
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Every use-package form installs its package; built-ins carry `:ensure nil'.
(setq use-package-always-ensure t)

;; NB: Bedrock also sets `frame-resize-pixelwise' here.  Deliberately not taken:
;; it lets the frame be sized to any pixel rather than a whole number of
;; character cells, so a maximized frame can carry a partial row/column that one
;; window absorbs -- and window geometry on this config is load-bearing (the
;; 2x2 startup layout at the end of init.el).  It buys us nothing otherwise.
(add-to-list 'default-frame-alist '(undecorated . t))

;;; early-init.el ends here
