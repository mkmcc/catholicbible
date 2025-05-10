;;; catholicbible-cli.el --- CLI interface to catholicbible.el
;; Usage: emacs --script catholicbible-cli.el TRANSLATION BOOK CHAPTER RANGE FORMAT

;; Example:
;;   emacs --script catholicbible-cli.el knox John 3 16-20 latex

(package-initialize)
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'catholicbible)

(defun catholicbible--parse-args ()
  "Parse command-line args into (translation book chapter range format)."
  (let ((args argv))
    (unless (>= (length args) 5)
      (error "Usage: emacs --script catholicbible-cli.el TRANSLATION BOOK CHAPTER RANGE FORMAT"))
    (list (nth 0 args)                           ;; translation
          (nth 1 args)                           ;; book
          (string-to-number (nth 2 args))        ;; chapter
          (nth 3 args)                           ;; range
          (nth 4 args))))                        ;; format

(let* ((args (catholicbible--parse-args))
       (translation (nth 0 args))
       (book (nth 1 args))
       (chapter (nth 2 args))
       (range (nth 3 args))
       (format (nth 4 args))
       (output
        (pcase format
          ("latex" (catholicbible-format-verses-latex translation book chapter range))
          ("text"  (catholicbible-format-verses-text  translation book chapter range))
          (_ (error "Unknown format: %s" format)))))
  (princ output)
  (terpri))
