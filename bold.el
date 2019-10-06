;;; bold.el --- A wrapper for multiple IDE backends -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)

(defgroup bold nil
  "A wrapper for multiple IDE backends.")

(defvar bold-mode-map (make-sparse-keymap))

(defcustom bold-minor-mode-alist
  '((tide-mode
     :package tide
     :major-modes (typescript-mode js-mode tsx-mode jsx-mode web-mode)
     :documentation-url "https://github.com/ananthakumaran/tide"
     :server-info tide-list-servers
     :server-restart tide-restart-server
     :documentation-at-point tide-documentation-at-point
     :find-references-at-point tide-references
     :fix-at-point tide-fix
     :format-region tide-format
     :list-file-errors nil
     :list-project-errors tide-project-errors
     :error-at-point tide-error-at-point
     :rename-file tide-rename-file
     :rename-symbol tide-rename-symbol
     :refactor tide-refactor
     :organize-imports tide-organize-imports
     :insert-block-comment tide-jsdoc-template
     :jump-to-definition tide-jump-to-definition
     :jump-back tide-jump-back
     ;; Unsupported commands:
     ;;
     ;; - tide-add-tslint-disable-next-line
     :priority 10))
  "Alist of minor modes supported by this backend."
  :group 'bold)

(defcustom bold-documentation-at-point-fallback
  #'describe-symbol
  "Fallback command/function from `bold-documentation-at-point'."
  :group 'bold
  :type 'function)

;;;; Common utilities
(defun bold--active-minor-modes ()
  (->> bold-minor-mode-alist
       (--filter (and (boundp (car it)) (symbol-value (car it))))
       (-sort (-on #'> (lambda (it) (plist-get (cdr it) :priority))))))

(defun bold--lookup-minor-property (property)
  (-some (lambda (cell) (plist-get (cdr cell) property))
         (bold--active-minor-modes)))

(cl-defmacro bold--def-minor-command (command property doc
                                              &rest fallback)
  (declare (indent 2))
  `(defun ,command ()
     ,doc
     (interactive)
     (let ((command (bold--lookup-minor-property ,property)))
       (cond
        (command (call-interactively command))
        (,fallback (cond ((and (symbolp ,fallback)
                               (fboundp ,fallback))
                          (if (commandp ,fallback)
                              (call-interactively ,fallback)
                            (funcall ,fallback)))
                         ((and (symbolp ,fallback)
                               (boundp ,fallback)
                               (fboundp (symbol-value ,fallback)))
                          (if (commandp (symbol-value ,fallback))
                              (call-interactively (symbol-value ,fallback))
                            (funcall (symbol-value ,fallback))))
                         (t
                          ,@fallback)))
        (t (user-error "No %s in any of the active minor modes" ,property))))))

;;;; Commands

;;;###autoload
(bold--def-minor-command bold-fix-at-point :fix-at-point
  "Apply fix for the error at point.")
;;;###autoload
(bold--def-minor-command bold-format-region :format-region
  "Format the region.")
;;;###autoload
(bold--def-minor-command bold-find-references-at-point :find-references-at-point
  "Find references to the symbol at point.")
;;;###autoload
(bold--def-minor-command bold-documentation-at-point :documentation-at-point
  "Display documentation on the symbol at point."
  'bold-documentation-at-point-fallback)
;;;###autoload
(bold--def-minor-command bold-server-info :server-info
  "Display the server information.")
;;;###autoload
(bold--def-minor-command bold-server-restart :server-restart
  "Restart the server.")
;;;###autoload
(bold--def-minor-command bold-list-file-errors :list-file-errors
  "Display a list of all errors in the file.")
;;;###autoload
(bold--def-minor-command bold-list-project-errors :list-project-errors
  "Display a list of all errors in the project.")
;;;###autoload
(bold--def-minor-command bold-error-at-point :error-at-point
  "Display details on the error at point.")
;;;###autoload
(bold--def-minor-command bold-rename-file :rename-file
  "Rename the current file.")
;;;###autoload
(bold--def-minor-command bold-rename-symbol :rename-symbol
  "Rename the symbol at point.")
;;;###autoload
(bold--def-minor-command bold-refactor :refactor
  "Refactor code at point or the region.")
;;;###autoload
(bold--def-minor-command bold-organize-imports :organize-imports
  "Organize imports in the file.")
;;;###autoload
(bold--def-minor-command bold-insert-block-comment :insert-block-comment
  "Insert a block comment for the current language.")
;;;###autoload
(bold--def-minor-command bold-jump-to-definition :jump-to-definition
  "Jump to the definition of the symbol at point."
  #'xref-find-definitions)
;;;###autoload
(bold--def-minor-command bold-jump-back :jump-back
  "Jump back to the previous location of `bold-jump-to-definition'."
  #'xref-pop-marker-stack)

;;;; Minor mode
(define-minor-mode bold-mode
  "Minor mode where commands in bold.el are enabled."
  nil " Bold" 'bold-mode-map)

(defun bold-mode-conditionally ()
  (interactive)
  (let ((enabled (-any (lambda (mode)
                         (and (boundp mode) (symbol-value mode)))
                       (-map #'car bold-minor-mode-alist))))
    (bold-mode enabled)))

;;;; Enabling
(defun bold-setup-hooks ()
  (cl-loop for (mode plist) in bold-minor-mode-alist
           do (let ((package (plist-get plist :package))
                    (hook (intern (format "%s-hook" mode))))
                (if package
                    (eval-after-load package
                      (add-hook hook #'bold-mode-conditionally))
                  (add-hook hook #'bold-mode-conditionally)))))

(provide 'bold)
;;; bold.el ends here
