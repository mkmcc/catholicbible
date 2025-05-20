;;; bibleapi.el ---   -*- lexical-binding: t; -*-
(provide 'bibleapi)

(require 'json)
(require 's)
(require 'dash)

(require 'bibleutils)

(defvar bibleapi--root-url
  "https://bible.helloao.org/")

(defun bibleapi--cache-path (url-path)
  (expand-file-name
   (s-chop-prefix "/" url-path)
   bible-cachedir))

(defun bibleapi--get-json (path)
  "Fetch JSON from PATH relative to API root."
  (let* ((url (format "%s/%s"
                      (s-chop-suffix "/" bibleapi--root-url)
                      (s-chop-prefix "/" path)))
         (body (bibleutils--get-html-body url (bibleapi--cache-path path))))
    (when body
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist :array-type 'list)))))

(defun bibleapi--get-available-translations ()
  "Fetch available Bible translations and pass the parsed result to CALLBACK."
  (alist-get
   'translations
   (bibleapi--get-json "api/available_translations.json")))

(defun print-table ()
  "Print a table of English Bible translations with 66 books."
  (let ((rows
         (--map (list (alist-get 'id it)
                      (alist-get 'name it)
                      (alist-get 'listOfBooksApiLink it))
                english-translations)))
    (with-output-to-temp-buffer "*Bible Translations*"
      (princ (format "%-10s %-50s %s\n" "ID" "Translation Name" "API Link"))
      (princ (make-string 100 ?-))
      (princ "\n")
      (--each rows
        (princ (format "%-10s %-50s %s\n"
                       (nth 0 it)
                       (nth 1 it)
                       (nth 2 it)))))))

(defun bibleapi--get-book-list (translation-id)
  "Fetch and return the list of books for TRANSLATION-ID."
  (let* ((entry (--find (equal (alist-get 'id it) translation-id)
                        (bibleapi--get-available-translations)))
         (link (alist-get 'listOfBooksApiLink entry)))
    (unless link
      (error "No listOfBooksApiLink found for translation ID: %s" translation-id))
    (alist-get 'books (bibleapi--get-json link))))

(defun bibleapi--print-books-table (books)
  "Pretty-print a table of BOOKS, which should be a list of alists with bookId and bookName."
  (with-output-to-temp-buffer "*Bible Books*"
    (princ (format "%-10s %s\n" "ID" "Book Name"))
    (princ (make-string 40 ?-))
    (princ "\n")
    (--each books
      (princ (format "%-10s %s\n"
                     (alist-get 'id it)
                     (alist-get 'name it))))))

(defun bibleapi--get-book-id (translation-id canonical-name)
  "Map the canonical book name to a book ID used by the API
returns a plist with keys :id and :nverse"
  (let* ((books (bibleapi--get-book-list translation-id))
         (entry (car
                 (--filter
                  (string= canonical-name (alist-get 'commonName it))
                  books))))
    `(:id ,(alist-get 'id entry)
          :nverse ,(alist-get 'totalNumberOfVerses entry))))

(defun bibleapi--get-chapter (translation-id book-id chapter-number)
  "Fetch CHAPTER-NUMBER of BOOK-ID from TRANSLATION-ID.
Returns a parsed alist containing the chapter text and metadata."
  (let ((path (format "api/%s/%s/%d.json"
                      translation-id
                      book-id
                      chapter-number)))
    (alist-get
     'content
     (alist-get
      'chapter
      (bibleapi--get-json path)))))

;; "In the beginning God created the heavens and the earth."
;; ((text . "So God created man in His own image;") (poem . 1))

(defun bibleapi--parse-content-field (item)
  (when (equal (alist-get 'type item) "verse")
    (let ((content (car (alist-get 'content item)))
          (number (alist-get 'number item)))
      (cond
       ((stringp content)
        `((type . "verse") (number . ,number) (content . ,content)))
       ((and (listp content)
             (alist-get 'text content)
             (alist-get 'poem content))
        `((type . "poem") (number . ,number) (content . ,(alist-get 'text content))))
       ))))

(defun bibleapi--fix-pillcrow (items)
  "Expand `¶` in verse content to an explicit line break element.
Returns a new list of content elements with inserted ((type . \"line_break\")) where appropriate."
  (--mapcat
   (if (and (or (equal (alist-get 'type it) "verse")
                (equal (alist-get 'type it) "poem"))
            (s-starts-with? "¶" (s-trim (alist-get 'content it))))
       (list '((type . "line_break")) it)
     (list it))
   items))

(defun bibleapi--fix-whitespace (items)
  (let* ((items1 (-map #'bibleapi--parse-content-field items))
         (items2 (bibleapi--fix-pillcrow items1)))
    (let ((result '())
          (last-verse nil))
      (dolist (el items2 result)
        (pcase (alist-get 'type el)
          ("verse"
           ;; Save the current verse and push any previous one
           (when last-verse
             (push last-verse result))
           (setq last-verse (copy-tree el)))
          ("poem"
           ;; Save the current verse and push any previous one
           (when last-verse
             (push last-verse result))
           (setq last-verse (copy-tree el)))
          ("line_break"
           ;; Append a newline to content of current verse
           (when last-verse
             (let ((content (alist-get 'content last-verse)))
               (setf (alist-get 'content last-verse)
                     (concat content "\n")))))
          (_ nil)))
      (reverse result))))

(defun bibleapi--get-verses-helper (translation-id canonical-book-name chapter-number range)
  (let* ((wanted (cond ((listp range) range)
                       (t (bible--expand-verse-spec range))))
         (book-data (bibleapi--get-book-id translation-id canonical-book-name))
         (book-id (plist-get book-data :id))
         (items (bibleapi--get-chapter translation-id book-id chapter-number))
         (verses (bibleapi--fix-whitespace items)))
    (--filter (member (alist-get 'number it) wanted) verses)))

(defun bibleapi--interleave-ellipsis (blocks)
  "Join list of BLOCKS (lists of verses) with (:ellipsis t) in between."
  (->> blocks
       (-interpose '(((type . "ellipsis"))))
       (-flatten-n 1)))

(defun bibleapi--get-verses (translation canonical-book-name chapter range)
  "Return verses from RANGE in TRANSLATION with ellipsis inserted between gaps."
  (let* ((wanted (bible--expand-verse-spec range))
         (all-verses (bibleapi--get-verses-helper translation canonical-book-name chapter wanted))
         (by-number (--group-by (alist-get 'number it) all-verses))
         (groups (bible--group-contiguous (mapcar #'car by-number)))
         (blocks (--map (mapcar (lambda (n) (car (alist-get n by-number))) it) groups)))
    (bibleapi--interleave-ellipsis blocks)))


;; public api

(defun bibleapi-format-verses-text (translation-name canonical-book-name chapter range)
  (let* ((translation
          (bible--normalize-translation translation-name))
         (verses
          (bibleapi--get-verses translation canonical-book-name chapter range))
         (lines
          (--map
           (pcase (alist-get 'type it)
             ("verse" (format "(%d) %s"
                              (alist-get 'number it)
                              (alist-get 'content it)))
             ("poem" (format "[poem] (%d) %s"
                              (alist-get 'number it)
                              (alist-get 'content it)))
             ("ellipsis" "\n...\n"))
           verses)))
    (s-replace-all
     '(("\n" . "\n\n")                  ;bsb
       ("¶" . ""))                      ;kjv
     (string-join (--filter it lines) " " ))))

(defconst bibleapi--translation-labels
  '(("eng_kja" . "KJV")
    ("eng_kjv" . "KJV")
    ("BSB"     . "BSB"))
  "Mapping of canonical translation names to display labels.")

(defun bibleapi--translation-label (name)
  "Return user-facing label (e.g., 'DRB') for canonical translation NAME."
  (let ((name_std (bible--normalize-translation name)))
    (or (cdr (assoc name_std bibleapi--translation-labels))
        name)))  ;; fallback: show canonical name if not found

(defun bibleapi--format-verse-latex (verse)
  "Format a single VERSE element as LaTeX."
  (pcase (alist-get 'type verse)
    ("verse"
     (format "\\vs{%d} %s"
             (alist-get 'number verse)
             (alist-get 'content verse)))
    ("poem"
     (format "\\vs{%d} %s"
             (alist-get 'number verse)
             (alist-get 'content verse)))
    ("ellipsis"
     "\n\\dots\n")
    (_ nil)))

(defun bibleapi--format-verses-latex-helper (verses)
  "Format VERSES into LaTeX lines, inserting poetry environment boundaries as needed."
  (let ((state 'prose)
        (result '()))
    (dolist (v verses)
      (pcase (alist-get 'type v)
        ("poem"
         (unless (eq state 'poetry)
           (push "\\begin{poetry}" result)
           (setq state 'poetry))
         (push (bibleapi--format-verse-latex v) result))
        ("verse"
         (when (eq state 'poetry)
           (push "\\end{poetry}" result)
           (setq state 'prose))
         (push (bibleapi--format-verse-latex v) result))
        (_
         ;; passthrough, no state change
         (push (bibleapi--format-verse-latex v) result))))
    ;; Final flush: if we're still in poetry, close the block
    (when (eq state 'poetry)
      (push "\\end{poetry}" result))
    (nreverse result)))

(defun bibleapi-format-verses-latex (translation-name book-name chapter range)
  "Format a scripture block for TRANSLATION-NAME, BOOK-NAME, CHAPTER, and RANGE using LaTeX."
  (let* ((translation
          (bible--normalize-translation translation-name))
         (canonical-book-name
          (bible--canonical-book-name book-name))
         (translation-label
          (bibleapi--translation-label translation))
         (translation-book-name
          (plist-get (bible--normalize-book translation canonical-book-name) :name))
         (titlestring
          (format "%s %s:%s (%s)" translation-book-name chapter range translation-label))
         (verses
          (bibleapi--get-verses translation canonical-book-name chapter range))
         (verselines
          (bibleapi--format-verses-latex-helper verses)))
    (string-join
     (list (format "\\begin{scripture}[%s]" titlestring)
           (s-replace "¶" "" (string-join verselines "\n"))
           "\\end{scripture}")
     "\n")))
