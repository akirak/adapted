;;; adapted-avy.el --- Avy interface -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (avy "0.5"))
;; Keywords: 
;; URL: https://github.com/akirak/adapted

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'adapted)
(require 'avy)
(require 'general)

(defcustom adapted-avy-prefix-key nil
  "Prefix key for adapted-avy commands."
  :type 'string
  :set (lambda (sym value)
         (when adapted-avy-prefix-key
           (general-unbind adapted-avy-prefix-key))
         (set-default sym value)
         (when value
           (general-define-key :prefix value
                               "j" #'avy-goto-char-timer
                               "s" '(nil :wk "symbol")
                               "sy" '(adapted-avy-insert-symbol :wk "insert")))))

(defun adapted-avy--symbol (op res)
  (let ((start (caar res))
        (window (cdr res)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char start)
        (let ((begin (if (looking-at (rx symbol-start))
                         (point)
                       (re-search-backward (rx symbol-start) nil t)))
              (end (save-excursion
                     (re-search-forward
                      (rx (group (+? anything)) symbol-end)
                      nil t))))
          (if op
              (funcall op begin end)
            (buffer-substring-no-properties begin end)))))))

;;;###autoload
(defun adapted-avy-insert-symbol ()
  "Kill-ring-save a symbol and insert it into the current point."
  (interactive)
  (let ((avy-all-windows t)
        (avy-pre-action (-partial #'adapted-avy--symbol #'kill-ring-save)))
    (save-excursion
      (avy-goto-char-timer))
    (yank)))

(provide 'adapted-avy)
;;; adapted-avy.el ends here
