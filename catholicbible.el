;;; catholicbible.el --- Bible verse fetching and formatting -*- lexical-binding: t; -*-

;; Interface for retrieving and formatting Bible verses from
;; https://catholicbible.online for multiple translations.

;; Depends on:
;; - elquery.el (HTML DOM parsing)
;; - dash.el
;; - s.el
;; - bible-translation-books.el


(provide 'catholicbible)

(require 'elquery)
(require 'url)
(require 's)
(require 'dash)

(require 'bible-normalize-input)
(require 'bible-translation-books)

(require 'bibleutils)


;;; Input Normalization

(defconst catholicbible--translation-labels
  '(("knox"         . "Knox")
    ("vulgate"      . "Vulgate")
    ("douay_rheims" . "DRB"))
  "Mapping of canonical translation names to display labels.")

(defun catholicbible--translation-label (name)
  "Return user-facing label (e.g., 'DRB') for canonical translation NAME."
  (let ((name_std (bible--normalize-translation name)))
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

;;; Caching and fetching

(defun catholicbible--chapter-url (translation url-path chapter)
  (format "https://catholicbible.online/%s/%s/ch_%d"
          translation url-path chapter))

(defun catholicbible--chapter-path (translation url-path chapter)
  (expand-file-name
   (format "%s/%s/ch_%d.html" translation url-path chapter)
   bible-cachedir))

(defun catholicbible--fetch-chapter-helper (translation url-path chapter)
  "Return elquery DOM of the requested chapter, caching it if needed.
TRANSLATION is standardized, CHAPTER is an integer"
  (let* ((url (catholicbible--chapter-url translation url-path chapter))
         (path (catholicbible--chapter-path translation url-path chapter)))
    (elquery-read-string
     (bibleutils--get-html-body url path))))

(defun catholicbible--fetch-chapter (translation canonical-book-name chapter)
  (let* ((pdata
          (bible--normalize-book translation canonical-book-name))
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

;;; Knox: should remove links
;; <div class="vers-content">After this, God said, Let the waters
;; produce moving things that have life in them, and winged things that
;; fly above the earth under the sky’s vault.<a href=""
;; class="inline-comment" data-comment-id="1">✻</a></div>

;;; Douay-Rheims: should not remove links
;; <div class="vers-content">And God blessed them, saying: <a href=""
;; class="inline-comment" data-comment-id="3672">Increase and
;; multiply</a>, and fill the earth, and subdue it, and rule over the
;; fishes of the sea, and the fowls of the air, and all living
;; creatures that move upon the earth.</div>

(defun catholicbible--content-text-knox (content-node)
  "Return cleaned text from CONTENT-NODE, stripping links and preserving <br> paragraph breaks."
  (let* ((children (catholicbible--strip-content-links content-node)) ; * comments
         (texts (-map #'catholicbible--full-text children)))
    (string-join texts "")))

(defun catholicbible--content-text-vulgate (content-node)
  "Return cleaned text from CONTENT-NODE, preserving <br> paragraph breaks."
  (let* ((children (plist-get content-node :children))
         (texts (-map #'catholicbible--full-text children)))
    (string-join texts "")))

(defun catholicbible--content-text-drb (content-node)
  "Return cleaned text from CONTENT-NODE, stripping links and preserving paragraph breaks."
  (elquery-full-text content-node))

(defun catholicbible--content-text (translation content-node)
  (cond
   ((string= translation "knox")
    (catholicbible--content-text-knox content-node))
   ((string= translation "douay_rheims")
    (catholicbible--content-text-drb content-node))
   ((string= translation "vulgate")
    (catholicbible--content-text-vulgate content-node))
   (t
    (error (format "Unknown translation %s" translation)))))

(defun catholicbible--parse-verse (translation verse)
  "Parse a single VERSE node and return (:verse N :text TEXT)."
  (let* ((no-node (car (elquery-$ "div.vers-no" verse)))
         (content-node (car (elquery-$ "div.vers-content" verse)))
         (verse-num (string-to-number (elquery-text no-node)))
         (text (catholicbible--content-text translation content-node)))
    `(:verse ,verse-num :text ,text)))

(defun catholicbible--parse-verses (translation dom)
  "Parse all verse nodes in DOM into a list of (:verse N :text ...) plists."
  (->> (elquery-$ "div.verses div.vers" dom)
       (-map (-partial #'catholicbible--parse-verse translation))
       (-sort (lambda (a b) (< (plist-get a :verse)
                               (plist-get b :verse))))))

;;; Public API

(defun catholicbible--get-verses-helper (translation canonical-book-name chapter range)
  "Return a list of verses matching RANGE from BOOK and CHAPTER in TRANSLATION.
RANGE may be a number, a string range, or a list of integers."
  (let* ((wanted (cond ((listp range) range)
                       (t (bible--expand-verse-spec range))))
         (dom (catholicbible--fetch-chapter translation canonical-book-name chapter))
         (verses (catholicbible--parse-verses translation dom)))
    (--filter (member (plist-get it :verse) wanted) verses)))

(defun catholicbible--interleave-ellipsis (blocks)
  "Join list of BLOCKS (lists of verses) with (:ellipsis t) in between.
Returns a flat list of plists."
  (->> blocks
       (-interpose '((:ellipsis t)))
       (-flatten-n 1)))

(defun catholicbible-get-verses (translation canonical-book-name chapter range)
  "Return verses from RANGE in TRANSLATION with ellipsis inserted between gaps."
  (let* ((wanted (bible--expand-verse-spec range))
         (all-verses (catholicbible--get-verses-helper translation canonical-book-name chapter wanted))
         (by-number (--group-by (plist-get it :verse) all-verses))
         (groups (bible--group-contiguous (mapcar #'car by-number)))
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
  (let* ((translation (bible--normalize-translation translation-name))
         (canonical-book-name (bible--canonical-book-name book-name))
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
          (bible--normalize-translation translation-name))
         (canonical-book-name
          (bible--canonical-book-name book-name))
         (translation-label
          (catholicbible--translation-label translation))
         (translation-book-name
          (plist-get (bible--normalize-book translation canonical-book-name) :name))
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
