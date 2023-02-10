;;; tssh-tramp.el --- TRAMP integration for docker containers  -*-

;;; Commentary:
;;

;;; Code:

;; tsh ssh method
(defun tssh-tramp-add-method ()
  "Add tsh ssh tramp method."
  (add-to-list 'tramp-methods
               `("tssh" .
                 ((tramp-login-program "tsh")
                  (tramp-login-args (("ssh") ("%h")))
                  (tramp-direct-async t)
                  (tramp-remote-shell "/bin/sh")
                  (tramp-remote-shell-login ("-l"))
                  (tramp-remote-shell-args ("-c")))
                 )
               ))

(eval-after-load 'tramp
  '(progn
     (tssh-tramp-add-method)
     ))


(provide 'tssh-tramp)

;;; tssh-tramp.el ends here
