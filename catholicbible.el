;;; catholicbible.el --- Bible verse fetching and formatting -*- lexical-binding: t; -*-

;; Core interface for retrieving and formatting Bible verses from
;; https://catholicbible.online for multiple translations.

;; Depends on:
;; - elquery.el (HTML DOM parsing)
;; - dash.el
;; - s.el
;; - catholicbible-books.el (book name mappings)

(provide 'catholicbible)

(require 'elquery)
(require 'url)
(require 's)
(require 'dash)

(require 'catholicbible-normalize-input)
(require 'catholicbible-translation-books)

(defvar catholicbible-cachedir
  (expand-file-name ".biblecache/" (getenv "HOME"))
  "Root directory for cached Bible chapters.")

;;; Input Normalization

(defconst catholicbible--translation-labels
  '(("knox"         . "Knox")
    ("vulgate"      . "Vulgate")
    ("douay_rheims" . "DRB"))
  "Mapping of canonical translation names to display labels.")

(defun catholicbible--translation-label (name)
  "Return user-facing label (e.g., 'DRB') for canonical translation NAME."
  (let ((name_std (catholicbible--normalize-translation name)))
      (or (cdr (assoc name_std catholicbible--translation-labels))
          name)))  ;; fallback: show canonical name if not found

(defun catholicbible--normalize-verse-range (range)
  "Normalize RANGE into a list of integers.
RANGE may be a number, a string like \"14-20\" or \"5\"."
  (cond
   ((numberp range) (list range))
   ((and (stringp range)
         (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" range))
    (let ((start (string-to-number (match-string 1 range)))
          (end (string-to-number (match-string 2 range))))
      (number-sequence start end)))
   ((and (stringp range)
         (string-match "^\\([0-9]+\\)$" range))
    (list (string-to-number (match-string 1 range))))
   (t (error "Invalid verse range: %S" range))))

;;; Range parsing

(defun catholicbible--expand-verse-spec (spec)
  "Expand comma-separated SPEC like \"3,5,7-10\" into a list of integers."
  (->> (s-split "," spec t)
       (-map #'catholicbible--normalize-verse-range)
       (-flatten)))

(defun catholicbible--group-contiguous (numbers)
  "Group NUMBERS into contiguous runs, e.g. (1 2 3 5) → ((1 2 3) (5))."
  (let ((sorted (sort (copy-sequence numbers) #'<))
        (groups '())
        (current-group '())
        (last nil))
    (dolist (n sorted)
      (if (or (null last) (= n (1+ last)))
          (push n current-group)
        (push (nreverse current-group) groups)
        (setq current-group (list n)))
      (setq last n))
    (when current-group
      (push (nreverse current-group) groups))
    (nreverse groups)))

;;; Caching and fetching

(defun catholicbible--chapter-url (translation url-path chapter)
  (format "https://catholicbible.online/%s/%s/ch_%d"
          translation url-path chapter))

(defun catholicbible--chapter-path (translation url-path chapter)
  (expand-file-name
   (format "%s/%s/ch_%d.html" translation url-path chapter)
   catholicbible-cachedir))

(defun catholicbible--ensure-dir (path)
  (make-directory (file-name-directory path) t))

(defun catholicbible--get-html-body-raw (url)
  "Fetch URL and return trimmed HTML body, skipping HTTP headers."
  (with-current-buffer (url-retrieve-synchronously url t t 10)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil 'move)
    (s-trim (buffer-substring-no-properties (point) (point-max)))))

(defun catholicbible--fix-encoding (html)
  (let* ((html1 (decode-coding-string html 'utf-8 t))
         (html2 (s-replace "\r" "" html1)))
    html2))

(defun catholicbible--get-html-body (url)
  (catholicbible--fix-encoding
   (catholicbible--get-html-body-raw url)))

(defun catholicbible--fetch-chapter-helper (translation url-path chapter)
  "Return elquery DOM of the requested chapter, caching it if needed.
TRANSLATION is standardized, CHAPTER is an integer"
  (let* ((url (catholicbible--chapter-url translation url-path chapter))
         (path (catholicbible--chapter-path translation url-path chapter)))
    (catholicbible--ensure-dir path)
    (if (file-exists-p path)
        (elquery-read-file path)
      (let ((html (catholicbible--get-html-body url)))
        (with-temp-buffer
          (set-buffer-file-coding-system 'utf-8)
          (insert html)
          (write-region (point-min) (point-max) path nil 'quiet))
        (elquery-read-string html)))))

(defun catholicbible--fetch-chapter (translation canonical-book-name chapter)
  (let* ((pdata
          (catholicbible--normalize-book translation canonical-book-name))
         (url-path (plist-get pdata :url-path))
         (name (plist-get pdata :name)))
    (catholicbible--fetch-chapter-helper translation url-path chapter)))

;;; Verse parsing

(defun catholicbible--strip-content-links (content-node)
  (--filter (not (and (listp it)
                      (equal (plist-get it :el) "a")))
            (plist-get content-node :children)))

(defun catholicbible--full-text (node &optional separator)
  "Like `elquery-full-text`, but renders <br> and <span class=\"content-paragraph\"> as newlines."
  (cond
   ((and (not (elquery-elp node)) (not (elquery-children node)))
    (or (plist-get node :text) ""))
   ((and (elquery-elp node)
         (equal (plist-get node :el) "br"))
    "\n")
   ;; ((and (elquery-elp node)
   ;;       (equal (plist-get node :el) "span")
   ;;       (let ((cls (plist-get node :class)))
   ;;         (or (equal cls "content-paragraph")
   ;;             (and (listp cls) (member "content-paragraph" cls)))))
   ;;  "\n\n")
   ((elquery-children node)
    (string-join
     (-remove #'string-empty-p
              (-map (lambda (child)
                      (catholicbible--full-text child separator))
                    (elquery-children node)))
     (or separator "")))
   (t "")))

(defun catholicbible--content-text (content-node)
  "Return cleaned text from CONTENT-NODE, stripping links and preserving paragraph breaks."
  (let* ((children (catholicbible--strip-content-links content-node))
         (texts (-map #'catholicbible--full-text children)))
    (string-join texts "")))

(defun catholicbible--parse-verse (verse)
  "Parse a single VERSE node and return (:verse N :text TEXT)."
  (let* ((no-node (car (elquery-$ "div.vers-no" verse)))
         (content-node (car (elquery-$ "div.vers-content" verse)))
         (verse-num (string-to-number (elquery-text no-node)))
         (text (catholicbible--content-text content-node)))
    `(:verse ,verse-num :text ,text)))

(defun catholicbible--parse-verses (dom)
  "Parse all verse nodes in DOM into a list of (:verse N :text ...) plists."
  (->> (elquery-$ "div.verses div.vers" dom)
       (-map #'catholicbible--parse-verse)
       (-sort (lambda (a b) (< (plist-get a :verse)
                               (plist-get b :verse))))))

;;; Public API

(defun catholicbible--get-verses-helper (translation canonical-book-name chapter range)
  "Return a list of verses matching RANGE from BOOK and CHAPTER in TRANSLATION.
RANGE may be a number, a string range, or a list of integers."
  (let* ((wanted (cond ((listp range) range)
                       (t (catholicbible--expand-verse-spec range))))
         (dom (catholicbible--fetch-chapter translation canonical-book-name chapter))
         (verses (catholicbible--parse-verses dom)))
    (--filter (member (plist-get it :verse) wanted) verses)))

(defun catholicbible--interleave-ellipsis (blocks)
  "Join list of BLOCKS (lists of verses) with (:ellipsis t) in between.
Returns a flat list of plists."
  (->> blocks
       (-interpose '((:ellipsis t)))
       (-flatten-n 1)))

(defun catholicbible-get-verses (translation canonical-book-name chapter range)
  "Return verses from RANGE in TRANSLATION with ellipsis inserted between gaps."
  (let* ((wanted (catholicbible--expand-verse-spec range))
         (all-verses (catholicbible--get-verses-helper translation canonical-book-name chapter wanted))
         (by-number (--group-by (plist-get it :verse) all-verses))
         (groups (catholicbible--group-contiguous (mapcar #'car by-number)))
         (blocks (--map (mapcar (lambda (n) (car (alist-get n by-number))) it) groups)))
    (catholicbible--interleave-ellipsis blocks)))

;;; Text Output

(defun catholicbible--format-verse-text (verse)
  (cond
   ((and (listp verse) (plist-get verse :ellipsis)) "\n…\n")
   ((plist-get verse :verse)
    (format "(%d) %s"
            (plist-get verse :verse)
            (plist-get verse :text)))
   (t "")))

(defun catholicbible-format-verses-text (translation-name book-name chapter range)
  "Format a list of verse plists and `(:ellipsis t)` markers into a natural paragraph string.
Verse texts are printed inline, with paragraph breaks preserved. Ellipses insert spacing."
  (let* ((translation (catholicbible--normalize-translation translation-name))
         (canonical-book-name (catholicbible--canonical-book-name book-name))
         (items
          (catholicbible-get-verses translation canonical-book-name chapter range)))
      (s-join "" (-map #'catholicbible--format-verse-text items))))


(defun catholicbible--format-verse-latex (verse)
  (cond
   ((and (listp verse) (plist-get verse :ellipsis)) "\n\n\\dots\n\n")
   ((plist-get verse :verse)
    (format "\\vs{%d}%s"
            (plist-get verse :verse)
            (plist-get verse :text)))
   (t "")))

(defun catholicbible-format-verses-latex (translation-name book-name chapter range)
  "Format a scripture block for TRANSLATION, BOOK, CHAPTER, and RANGE using the LaTeX scripture package."
  (let* ((translation
          (catholicbible--normalize-translation translation-name))
         (canonical-book-name
          (catholicbible--canonical-book-name book-name))
         (translation-label
          (catholicbible--translation-label translation))
         (translation-book-name
          (plist-get (catholicbible--normalize-book translation canonical-book-name) :name))
         (titlestring
          (format "%s %s:%s (%s)" translation-book-name chapter range translation-label))
         (verses
          (catholicbible-get-verses translation canonical-book-name chapter range))
         (verselines (-map #'catholicbible--format-verse-latex verses)))
    (string-join
     (list (format "\\begin{scripture}[%s]" titlestring)
           (string-join verselines "\n")
           "\\end{scripture}")
     "\n")))
