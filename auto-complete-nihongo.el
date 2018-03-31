(defvar ac-nihongo-separator-regexp "[[:ascii:]、。・「」：？…（）\n\t]"
  "Default separator to split string in buffers. Characters in
  this regexp are excluded from candidates.")

(defvar ac-nihongo--index-cache-alist nil
  "Alist in the form of (buffer . hashtable).")

(defvar ac-nihongo-select-buffer-function #'ac-nihongo-select-exact-same-mode-buffers)

(defvar ac-nihongo--hashtable-key-length 1)

(defun ac-nihongo--make-category-table (category)
  "Return a category table in which characters in chars list
below no longer belong to category CATEGORY."
  (cl-loop with table = (copy-category-table)
           with chars = '("ー" "～")
           for ch in (mapcar #'string-to-char chars)
           ;; Remove these characters from CATEGORY.
           do (modify-category-entry ch category table t)
           finally return table))

(defvar ac-nihongo--hiragana-category-table (ac-nihongo--make-category-table ?K)
  "Category table used to extract hiragana words.")

(defvar ac-nihongo--katakana-category-table (ac-nihongo--make-category-table ?H)
  "Category table used to extract katakana words.")

(defun ac-nihongo-select-exact-same-mode-buffers ()
  (cl-remove-if-not
   (lambda (buf) (eq major-mode (buffer-local-value 'major-mode buf)))
   (buffer-list)))

(defun ac-nihongo-get-candidates ()
  (when ac-prefix
    (cl-loop for buf in (funcall ac-nihongo-select-buffer-function)
             nconc (ac-nihongo-get-candidates-1 ac-prefix buf) into candidates
             finally return (delete-dups (sort candidates #'string<)))))

(defun ac-nihongo-get-candidates-in-current-buffer (prefix)
  ;; Todo: decide whether we need to update hashtable.
  ;; using timer? or any other threshold? or use dabbrev only in the current buffer?
  (cl-loop for word in (ac-nihongo-split-string (buffer-substring-no-properties
                                                 (point-min) (point-max)))
           when (string-match-p (concat "^" prefix) word)
           collect word into candidates
           finally return candidates))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  (when prefix
    (cl-loop for cands in (gethash (substring-no-properties prefix
                                                            0
                                                            ac-nihongo--hashtable-key-length)
                                   (ac-nihongo-get-hashtable buf))
             when (string-match-p (format "^%s" prefix) cands)
             collect cands into candidates
             finally return candidates)))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  (cond
   ((eq buf (current-buffer))
    ;; dynamically collect words in current buffer, and let's see how long it takes.
    ;; => takes too long to bear with.
    (ac-nihongo-get-candidates-in-current-buffer prefix))
   (t
    (cl-loop for cands in (gethash (substring-no-properties prefix
                                                            0
                                                            ac-nihongo--hashtable-key-length)
                                   (ac-nihongo-get-hashtable buf))
             when (string-match-p (format "^%s" prefix) cands)
             collect cands into candidates
             finally return candidates))))

(defun ac-nihongo-get-hashtable (buf)
  (unless (assoc buf ac-nihongo--index-cache-alist)
    ;; Make a new hashtable for this buffer. Key is a string of length
    ;; ac-nihongo--hashtable-key-length and its value is a list of
    ;; strings, i.e. "あ" => ("あい" "あお" "あほ" "あんこ"...)
    (cl-loop with table = (make-hash-table :test #'equal)
             with inserted-words = (make-hash-table :test #'equal)
             for word in (with-current-buffer buf
                           (ac-nihongo-split-string
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
             for key = (substring word 0 1)
             when (> (length word) 1)
             if (gethash key table)
             do (unless (gethash word inserted-words)
                  ;; this word has not inserted yet
                  (push word (gethash key table)))
             else
             do (puthash key (list word) table)
             finally (push (cons buf table) ac-nihongo--index-cache-alist)))
  (assoc-default buf ac-nihongo--index-cache-alist))

(defun ac-nihongo-split-string (s)
  (cl-remove-duplicates
   (nconc
    ;; 2-byte hiragana
    (with-category-table ac-nihongo--hiragana-category-table
      (split-string s (concat ac-nihongo-separator-regexp "\\|\\cK\\|\\cC") t))
    ;; 2-byte katakana
    (with-category-table ac-nihongo--katakana-category-table
      (split-string s (concat ac-nihongo-separator-regexp "\\|\\cH\\|\\cC") t))
    ;; 2-byte kanji
    (split-string s (concat ac-nihongo-separator-regexp "\\|\\cH\\|\\cK") t))
   :test #'string=))

(defun ac-dabbreg-get-regexp ()
  (unless (bobp)
    (cl-loop with s = (char-to-string (char-before))
             for pair in ac-dabbreg-regexp-cons-list
             when (string-match-p (car pair) s)
             return pair)))

(defun ac-nihongo-get-regexp ()
  (let ((ch (char-to-string (char-before))))
    ;; Bug: When ch is "～", both hiraganra and katakana will be
    ;; correct.  How do we decide which regexp we use?
    (cond
     ((string-match-p "\\cH" ch) "\\cH")
     ((string-match-p "\\cK" ch) "\\cK")
     ((string-match-p "\\cC" ch) "\\cC")
     (t nil))))

(defun ac-nihongo-prefix ()
  (let ((regexp (ac-nihongo-get-regexp)))
    (when regexp
      (save-excursion
        (backward-char)
        (while (and (not (bobp))
                    (looking-at-p regexp))
          (backward-char))
        (if (looking-at-p regexp)
            (point)
          (1+ (point)))))))

(defun ac-nihongo-init ()
  (interactive)
  (setq ac-sources '(ac-source-nihongo)))

(ac-define-source nihongo
  '((candidates . ac-nihongo-get-candidates)
    (prefix . ac-nihongo-prefix)
    (requires . 0)))
