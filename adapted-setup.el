;;; adapted-setup.el --- Setup adapted -*- lexical-binding: t -*-

(defvar adapted-minor-mode-alist)
(declare-function 'adapted-mode-conditionally "adapted")

(defun adapted-setup-hooks ()
  "Configure hooks to activate `adapted-mode'."
  (cl-loop for (mode plist) in adapted-minor-mode-alist
           do (let ((package (plist-get plist :package))
                    (hook (intern (format "%s-hook" mode))))
                (if package
                    (eval-after-load package
                      (add-hook hook #'adapted-mode-conditionally))
                  (add-hook hook #'adapted-mode-conditionally)))))

(provide 'adapted-setup)
;;; adapted-setup.el ends here
