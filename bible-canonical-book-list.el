;;; bible-canonical-book-list.el ---  -*- lexical-binding: t; -*-

(provide 'bible-canonical-book-list)

(defconst bible-canonical-list
  '(;; --- Old Testament ---
    "Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth"
    "1 Samuel" "2 Samuel" "1 Kings" "2 Kings" "1 Chronicles" "2 Chronicles"
    "Ezra" "Nehemiah" "Tobit" "Judith" "Esther" "1 Maccabees" "2 Maccabees"
    "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Songs" "Wisdom" "Sirach"
    "Isaiah" "Jeremiah" "Lamentations" "Baruch" "Ezekiel" "Daniel" "Hosea"
    "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah"
    "Haggai" "Zechariah" "Malachi"
    ;; --- New Testament ---
    "Matthew" "Mark" "Luke" "John" "Acts"
    "Romans" "1 Corinthians" "2 Corinthians" "Galatians" "Ephesians"
    "Philippians" "Colossians" "1 Thessalonians" "2 Thessalonians"
    "1 Timothy" "2 Timothy" "Titus" "Philemon" "Hebrews" "James"
    "1 Peter" "2 Peter" "1 John" "2 John" "3 John" "Jude"
    "Revelation")
  "Canonical names and order of Bible books from the USCCB website:
https://www.usccb.org/offices/new-american-bible/books-bible")
