;;; kjv.el --- Retrieve KJV Bible verses via bible-api.com -*- lexical-binding: t; -*-

(provide 'kjv)

(require 'json)
(require 'url)
(require 'url-util)
(require 's)
(require 'dash)

(defun kjv--url (book chapter range)
  "Construct a bible-api.com URL for BOOK, CHAPTER, and RANGE."
  (let ((ref (format "%s %s:%s" book chapter range)))
    (format "https://bible-api.com/%s?translation=kjv&single_chapter_book_matching=indifferent"
            (url-hexify-string ref))))

(defun kjv--fetch-json (book chapter range)
  "Fetch and parse JSON from bible-api.com for BOOK CHAPTER RANGE."
  (let ((buf (url-retrieve-synchronously (kjv--url book chapter range) t t 10)))
    (unless (buffer-live-p buf)
      (error "Failed to fetch passage: %s %s:%s" book chapter range))
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun kjv-format-verses-text (book chapter range)
  "Return plain-text KJV verses for BOOK CHAPTER RANGE."
  (let* ((data (kjv--fetch-json book chapter range))
         (verses (alist-get 'verses data)))
    (->> verses
         (--map (format "(%d) %s"
                                        ;(alist-get 'chapter it)
                        (alist-get 'verse it)
                        (alist-get 'text it)))
         (s-join ""))))

(defun kjv-format-verses-latex (book chapter range)
  "Return LaTeX-formatted KJV verses for BOOK CHAPTER RANGE."
  (let* ((data (kjv--fetch-json book chapter range))
         (verses (alist-get 'verses data))
         (titlestring
          (format "%s %s:%s (KJV)" book chapter range))
         (body
          (->> verses
               (--map (format "\\vs{%d}%s"
                                        ;(alist-get 'chapter it)
                              (alist-get 'verse it)
                              (alist-get 'text it)))
               (s-join ""))))

    (format "\\begin{scripture}[%s]\n%s\n\\end{scripture}"
            titlestring
            body)))
