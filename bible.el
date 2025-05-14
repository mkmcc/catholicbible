;;; bible.el --- Unified Bible interface -*- lexical-binding: t; -*-

;; Depends on:
;; - elquery.el (HTML DOM parsing)
;; - dash.el
;; - s.el
;; - bible-translation-books.el

(provide 'bible)

(require 'catholicbible)
(require 'esv)
(require 'esv-secrets)

(defun bible--which-backend (translation)
  "Return the backend symbol ('catholic or 'esv) for TRANSLATION."
  (let ((canon (downcase translation)))
    (cond
     ((member canon '("knox" "vulgate" "douay_rheims")) 'catholic)
     ((string= canon "esv") 'esv)
     (t (error "Unknown translation: %s" translation)))))

;;; Unified API

(defun bible-format-verses-text (translation book chapter range)
  "Return plain-text version of the verses."
  (pcase (bible--which-backend translation)
    ('catholic (catholicbible-format-verses-text translation book chapter range))
    ('esv      (esv-format-verses-text (format "%s %d:%s" book chapter range)))))

(defun bible-format-verses-latex (translation book chapter range)
  "Return LaTeX-formatted version of the verses."
  (pcase (bible--which-backend translation)
    ('catholic (catholicbible-format-verses-latex translation book chapter range))
    ('esv      (esv-format-verses-latex (format "%s %d:%s" book chapter range)))))

(defun bible-insert-verses-latex (translation-name book-name chapter range)
  "Prompt for TRANSLATION-NAME, BOOK-NAME, CHAPTER, and RANGE.
Then insert LaTeX-formatted verses at point."
  (interactive
   (let ((completion-ignore-case t))  ;; makes all completions case-insensitive
     (let* ((translation
             (completing-read "Translation: " '("knox" "douay_rheims" "vulgate" "esv") nil t))
            (book
             (completing-read "Book name: " catholicbible-canonical-list nil t))
            (max-ch
             (bible--get-chapnum translation book))
            (chapter-str (completing-read
                          (format "Chapter (1-%d): " max-ch)
                          (mapcar #'number-to-string (number-sequence 1 max-ch))
                          nil t))
            (range
             (read-string "Verse range (e.g. 4 or 1-5 or 1,3,7-10): ")))
       (list translation book (string-to-number chapter-str) range))))
  (insert
   (bible-format-verses-latex translation-name book-name chapter range)))

(defun bible--setup-latex-bindings ()
  (local-set-key (kbd "C-c v") #'bible-insert-verses-latex))

(add-hook 'latex-mode-hook #'bible--setup-latex-bindings)
