;;; skip-sexp.el --- Skip sexp in elisp mode

;; Copyright (C) 2013  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code uses the "skip N characters" of Emacs (#@N...) to implement
;; comment-like behavior. This is similar to what other lisps offer as #;,
;; although less nicer.

;;; Code:

(defun skip-sexp ()
  (interactive)
  (let (beg end)
    (save-excursion
      (mark-sexp)
      (goto-char
       (setq end (mark)))
      (backward-sexp)
      (setq beg (point)))
    (let ((l (string-bytes (buffer-substring beg end))))
      (save-excursion
        (let ((prefix (format "#@%d." (1+ l))))
          (goto-char beg)
          (insert prefix)
          (skip-sexp-install-overlays
           beg (+ beg (length prefix))
           (progn (forward-sexp) (point))))))))

(defun skip-sexp-detect-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "#@" (1+ digit) ".") nil t)
      (skip-sexp-install-overlays
       (match-beginning 0) (match-end 0) (progn (mark-sexp) (mark))))))

(put 'skip-sexp-prefix-category 'face 'font-lock-warning-face)
(put 'skip-sexp-prefix-category 'intangible t)
(put 'skip-sexp-sexp-category 'face 'font-lock-comment-face)

(defun skip-sexp-refresh (overlay after beg end &optional prev-length)
  (when after
    (let ((prefix (overlay-get overlay 'skip-sexp-prefix))
          (size (- (overlay-end overlay)
                   (overlay-start overlay))))
      (save-excursion
        (goto-char (overlay-start prefix))
        (re-search-forward (rx (1+ digit)) nil t)
        (replace-match (format "%d" (1+ size)) nil nil)))))

(defun skip-sexp-install-overlays (beg sbeg send)
  (let ((prefix (make-overlay beg sbeg nil t nil))
        (sexp (make-overlay sbeg send nil t nil)))
    (overlay-put prefix 'category 'skip-sexp-prefix-category)
    (overlay-put sexp 'category 'skip-sexp-sexp-category)
    (overlay-put sexp 'skip-sexp-prefix prefix)
    (overlay-put sexp 'modification-hooks '(skip-sexp-refresh))))

#@16.(+ 1 2 3 plop)
(provide 'skip-sexp)
;;; skip-sexp.el ends here
