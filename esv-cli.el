;;; esv-cli.el --- CLI interface to ESV Bible retrieval -*- lexical-binding: t; -*-

(package-initialize)
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'esv)

(require 'esv-secrets)

(defun esv-cli--parse-args ()
  "Parse command-line args into (translation book chapter range format)."
  (let ((args argv))
    (unless (>= (length args) 5)
      (error "Usage: emacs --script catholicbible-cli.el TRANSLATION BOOK CHAPTER RANGE FORMAT"))
    (list (nth 0 args)                           ;; translation
          (nth 1 args)                           ;; book
          (string-to-number (nth 2 args))        ;; chapter
          (nth 3 args)                           ;; range
          (nth 4 args))))                        ;; format

(let* ((args (esv-cli--parse-args))
       (translation (nth 0 args))
       (book    (nth 1 args))
       (chapter (nth 2 args))
       (range   (nth 3 args))
       (format  (nth 4 args))
       (reference (format "%s %d:%s" book chapter range))
       (output
        (pcase format
          ("latex" (esv-format-passage-latex reference))
          ("text"  (esv--fetch-text reference))
          (_ (error "Unknown format: %s" format)))))
  (princ output)
  (terpri))
