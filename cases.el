;;; $DOOMDIR/cases.el -*- lexical-binding: t; -*-


;;
;;; helper functions

(defun drmgc/space-case (input)
  "Convert any string to foo bar (spaces separated) format."
  (let* ((case-fold-search nil)
         (case-separated (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" input))
         (delimiter-separated (replace-regexp-in-string "[-_\. ]+" " " case-separated)))
    (downcase delimiter-separated)))
