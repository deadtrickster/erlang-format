;;; erlang-format.el --- Emacs plugin to erlang_ls -i Erlang files
;; Version: 0.1.0

;; Copyright (C) 2018 Ilya Khaprov

;; Author: Ilya Khaprov <i.khaprov@gmail.com>
;; URL: https://github.com/deadtrickster/erlang-format

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; The erlang-format function formats the erlang files with Erlang's `https://github.com/erlang/sourcer`
;; Based on https://github.com/anildigital/mix-format

;; e.g.
;;
;; (require 'erlang-format)
;; M-x erlang-format
;;

(defcustom erlfmt-erlang-ls "erlang_ls"
  "Path to the 'erlang_ls' executable."
  :type 'string
  :group 'erlang-format)

(defcustom erlfmt-args nil
  "Additional arguments to 'erlang_ls -i'"
  :type '(repeat string)
  :group 'erlang-format)

(defcustom erlang-format-hook nil
  "Hook called by `erlang-format'."
  :type 'hook
  :group 'erlang-format)


;;; Code

;;;###autoload
(defun erlang-format-before-save ()
  "Add this to .emacs to run erlang_ls -i on the current buffer when saving:
\(add-hook 'before-save-hook 'erlang-format-before-save).

Note that this will cause ‘erlang-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'erlang-mode) (erlang-format)))


(defun erlfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun erlfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun erlfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.
Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"

  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in erlfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (erlfmt--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (erlfmt--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in erlfmt--apply-rcs-patch"))))))))
  )

;;;###autoload
(defun erlang-format (&optional is-interactive)
  (interactive "p")

  (let ((outbuff (get-buffer-create "*erlang-format-output*"))
        (errbuff (get-buffer-create "*erlang-format-errors*"))
        (tmpfile (make-temp-file "erlang-format" nil ".erl"))
        (our-erlfmt-args (list "-i"))
        (output nil))

    (unwind-protect
        (save-restriction
          (with-current-buffer outbuff
            (erase-buffer))

          (with-current-buffer errbuff
            (setq buffer-read-only nil)
            (erase-buffer))

          (write-region nil nil tmpfile)

          (run-hooks 'erlang-format-hook)

          (when erlfmt-args
            (setq our-erlfmt-args (append our-erlfmt-args erlfmt-args)))
          (setq our-erlfmt-args (append our-erlfmt-args (list tmpfile)))

          (if (zerop (apply #'call-process erlfmt-erlang-ls nil errbuff t our-erlfmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil outbuff nil "-n" "-" tmpfile))
                    (message "File is already formatted")
                  (progn
                    (erlfmt--apply-rcs-patch outbuff)
                    (message "erlang-ls -i applied")))
                (kill-buffer errbuff))

            (progn
              (with-current-buffer errbuff
                (setq buffer-read-only t)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode))

              (if is-interactive
                  (display-buffer errbuff)
                (message "erlang-format failed: see %s" (buffer-name errbuff)))))

          ;;(delete-file tmpfile)
          (kill-buffer outbuff)))))

(provide 'erlang-format)

;;; erlang-format.el ends here
