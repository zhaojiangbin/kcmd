;;; kcmd.el --- Kill/copy/move/dup region or line range

;; Copyright (C) 2016 Jiangbin Zhao

;; Author: Jiangbin Zhao (zhaojiangbin@gmail.com)
;; Version: 0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs

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

;; Usage:
;;
;; (load "/path/to/kcmd")
;; (require 'kcmd)
;; (global-set-key "C-c m" 'kcmd-hydra/body)


(require 'move-dup)
(require 'avy)
(require 'hydra)

;;;;;; public ;;;;;;

(defface kcmd-buffer-name
  '((((background dark)) (:foreground "gold"))
    (((background light)) (:foreground "brown")))
  "Face used by kcmd.

For highlighting which window avy to select line range in.")

(defgroup kcmd nil
  "Kill/copy/move/dup active region or by avy line range."
  :prefix "kcmd-"
  :group 'tools)

(defcustom kcmd-buffer-name-face 'kcmd-buffer-name
  "In the window that avy line range is to be pick, show the
window's buffer name in this face."
  :group 'kcmd
  :type 'face)

(defcustom kcmd-buffer-name-max-length 45
  "The max length of buffer name to show when another window is
selected as source of avy line range, including length of the
string given in `kcmd-buffer-name-shorten-filler'."
  :group 'kcmd
  :type 'integer)

(defcustom kcmd-buffer-name-shorten-style 'middle
  "Controls how to shorten buffer names longer than
`kcmd-buffer-name-max-length'. The deleted portion is replaced
with the string given in
`kcmd-buffer-name-shorten-filler'. Possible choices:

  `middle'    -- delete a portion in the middle
  `beginning' -- delete the beginning portion
  `end'       -- delete the end portion"
  :group 'kcmd
  :type '(choice (const :tag "Middle" middle)
                 (const :tag "Beginning" beginning)
                 (const :tag "End" end)))

(defcustom kcmd-buffer-name-shorten-filler "..."
  "The filler string that replaces the truncated portion from long
buffer names."
  :group 'kcmd
  :type 'string)

(defcustom kcmd-hydra-run-body-pre 'nil
  "The function to run in the kcmd hydra's :body-pre hook. If you
customize this variable, you might want to look at
`kcmd-hydra-run-before-exit', too."
  :group 'kcmd
  :type 'function)

(defcustom kcmd-hydra-run-before-exit 'nil
  "The function to run in the kcmd hydra's :before-exit hook. If you
customize this variable, you might want to look at
`kcmd-hydra-run-body-pre', too."
  :group 'kcmd
  :type 'function)

;;;;; private ;;;;;;

;; The window in which avy selects the lines/region to copy/kill.
(defvar kcmd--avy-win nil)

(defmacro kcmd--avy-with-range (cmd arg &rest body)
  (declare (indent defun))
  ;; cmd determines the `avy-keys' and `avy-style'.
  `(avy-with ',cmd
     (save-selected-window
       ;; Value in `kcmd--avy-win' can be the selected window.
       (select-window kcmd--avy-win)
       (let* ((val (if ,arg (prefix-numeric-value ,arg)))
              (beg (avy--line))
              (end (if ,arg
                       (save-excursion
                         (goto-char beg)
                         (move-end-of-line val)
                         (line-end-position))
                     (avy--line)))
              (num (if ,arg val (count-lines beg end))))
         ,@body
         num))))

(defmacro kcmd--def1 (name func)
  (declare (indent defun))
  `(defun ,name (&optional arg)
     (interactive "P")
     (kcmd--avy-with-range avy-move-line arg
       (save-excursion
         (goto-char end)
         ;; NOTE: If `line-end-position' was used here, the newline
         ;; following end would be left unkilled.
         (move-beginning-of-line 2)
         (,func beg (point))))))

(kcmd--def1 kcmd--avy-copy  copy-region-as-kill)
(kcmd--def1 kcmd--avy-kill  kill-region)
(kcmd--def1 kcmd--avy-erase delete-region)

(defmacro kcmd--def2 (name func)
  (declare (indent defun))
  `(defun ,name (&optional arg)
     (interactive "P")
     (,func arg)
     (beginning-of-line)
     (insert (current-kill 0))))

(kcmd--def2 kcmd--avy-dup  kcmd--avy-copy)
(kcmd--def2 kcmd--avy-move kcmd--avy-kill)

;; `avy-with' throws `user-error' if cancelled with C-g. Catch this
;; error to reduce noises.
(defun kcmd--avy-run (cmd)
  (condition-case
      err
      (progn
        (call-interactively cmd))
    (user-error nil)
    (quit nil)))

(defun kcmd--add-face (str face &optional start end)
  (let ((start (or start 0))
        (end   (or end (length str))))
    (add-face-text-property start end face nil str))
  str)

(defun kcmd--strdup (str &optional beg end)
  (substring-no-properties str beg end))

(defun kcmd--fmt-buffer-name (name)
  (if (< (length name) kcmd-buffer-name-max-length)
      name
    (let* ((maxl kcmd-buffer-name-max-length)
           (styl kcmd-buffer-name-shorten-style)
           (fstr kcmd-buffer-name-shorten-filler)
           (flen (length fstr))
           (keep (- maxl flen)))
      (cond
       ((eq styl 'middle)
        (setq keep (/ keep 2))
        (concat (kcmd--strdup name 0 keep)
                fstr
                (kcmd--strdup name (- keep))))
       ((eq styl 'end)
        (concat (kcmd--strdup name 0 keep)
                fstr))
       ((eq styl 'beginning)
        (concat fstr
                (kcmd--strdup name (- keep))))))))

(defun kcmd--fmt-avy-win ()
  ;; NOTE: kcmd--avy-win should always point at a live window so there
  ;; is always a buffer in that window.
  (if (eq kcmd--avy-win (selected-window))
      (kcmd--add-face "this" kcmd-buffer-name-face)
    (let* ((name (buffer-name (window-buffer kcmd--avy-win))))
      (concat (kcmd--add-face "other" kcmd-buffer-name-face)
              " "
              (kcmd--fmt-buffer-name name)))))

(defun kcmd--select-avy-win ()
  (interactive)
  (let ((that-win (next-window kcmd--avy-win nil 'visible)))
    (if (one-window-p nil 'visible)
        (message "No other window available.")
      (setq kcmd--avy-win that-win))))

(defmacro kcmd--safe-call (func)
  `(when (and ,func (functionp ,func))
     (funcall ,func)))

(defhydra kcmd-hydra
  (:exit nil :hint nil
   :foreign-keys run
   :body-pre (progn
               (setq kcmd--avy-win (selected-window))
               (kcmd--safe-call kcmd-hydra-run-body-pre))
   :before-exit (kcmd--safe-call kcmd-hydra-run-before-exit))
  "
move/duplicate current line/active region
  _n_/_N_: down  _p_/_P_: up

select avy line range in _w_indow: %s(kcmd--fmt-avy-win)
  _e_: erase  _k_: kill  _c_: copy  _m_: move  _d_: duplicate

"
  ("N" md/duplicate-down)
  ("P" md/duplicate-up)
  ("n" md/move-lines-down)
  ("p" md/move-lines-up)
  ("k" (kcmd--avy-run 'kcmd--avy-kill))
  ("c" (kcmd--avy-run 'kcmd--avy-copy))
  ("e" (kcmd--avy-run 'kcmd--avy-erase))
  ("m" (kcmd--avy-run 'kcmd--avy-move))
  ("d" (kcmd--avy-run 'kcmd--avy-dup))
  ("w" kcmd--select-avy-win)
  ("<escape>" nil "quit"))

(provide 'kcmd)