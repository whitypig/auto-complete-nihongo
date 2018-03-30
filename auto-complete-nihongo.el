(defvar ac-nihongo-separator-regexp "[[:ascii:]、。・「」\n\t]"
  "Default separator to split string in buffers. Characters in
  this regexp are excluded from candidates.")

(defvar ac-nihongo--index-cache-alist nil
  "Alist in the form of (buffer . hashtable).")

(defvar ac-nihongo-select-buffer-function #'ac-nihongo-select-exact-same-mode-buffers)

(defvar ac-nihongo--hashtable-key-length 1)

(defun ac-nihongo-select-exact-same-mode-buffers ()
  (cl-remove-if-not
   (lambda (buf) (eq major-mode (buffer-local-value 'major-mode buf)))
   (buffer-list)))

(defun ac-nihongo-get-candidates ()
  (when ac-prefix
    (cl-loop for buf in (funcall ac-nihongo-select-buffer-function)
             nconc (ac-nihongo-get-candidates-1 ac-prefix buf) into candidates
             finally return (cl-remove-duplicates
                             (sort candidates #'string<)
                             :test #'string=))))

(defun ac-nihongo-get-candidates-in-current-buffer (prefix)
  (cl-loop for word in (ac-nihongo-split-string (buffer-substring-no-properties
                                                 (point-min) (point-max)))
           when (string-match-p (concat "^" prefix) word)
           collect word into candidates
           finally return candidates))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  (if (eq buf (current-buffer))
      ;; dynamically collect words in current buffer, and let's see how long it takes.
      (ac-nihongo-get-candidates-in-current-buffer prefix)
    (cl-loop for cands in (gethash (substring-no-properties prefix
                                                            0
                                                            ac-nihongo--hashtable-key-length)
                                   (ac-nihongo-get-hashtable buf))
             when (string-match-p (format "^%s" prefix) cands)
             collect cands into candidates
             finally return candidates)))

(defun ac-nihongo-get-hashtable (buf)
  (unless (assoc buf ac-nihongo--index-cache-alist)
    ;; Make a new hashtable for this buffer. Key is string of length
    ;; ac-nihongo--hashtable-key-length and value is a list of
    ;; strings.
    (cl-loop with table = (make-hash-table :test #'equal)
             for word in (with-current-buffer buf
                           (ac-nihongo-split-string
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
             for key = (substring word 0 1)
             when (> (length word) 1)
             do (if (gethash key table)
                    (push word (gethash key table))
                  (puthash key (list word) table))
             finally (push (cons buf table) ac-nihongo--index-cache-alist)))
  (assoc-default buf ac-nihongo--index-cache-alist))

(defun ac-nihongo-split-string (s)
  (cl-remove-duplicates
   (nconc
    ;; hiragana
    (split-string s (concat ac-nihongo-separator-regexp "\\|\\cK\\|\\cC") t)
    ;; katakana
    (split-string s (concat ac-nihongo-separator-regexp "\\|\\cH\\|\\cC") t)
    ;; kanji
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
