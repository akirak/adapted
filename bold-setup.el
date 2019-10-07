;;; bold-setup.el --- Setup bold.el -*- lexical-binding: t -*-

(defvar bold-minor-mode-alist)
(declare-function 'bold-mode-conditionally "bold")

(defun bold-setup-hooks ()
  "Configure hooks to activate `bold-mode'."
  (cl-loop for (mode plist) in bold-minor-mode-alist
           do (let ((package (plist-get plist :package))
                    (hook (intern (format "%s-hook" mode))))
                (if package
                    (eval-after-load package
                      (add-hook hook #'bold-mode-conditionally))
                  (add-hook hook #'bold-mode-conditionally)))))

(provide 'bold-setup)
;;; bold-setup.el ends here
