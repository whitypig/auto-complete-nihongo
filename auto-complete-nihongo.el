(defvar ac-nihongo-separator-regexp "[[:ascii:]、。・ー～]"
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

(defun ac-nihongo-get-candidates (prefix)
  (cl-loop for buf in (funcall 'ac-nihongo-select-exact-same-mode-buffers)
           collect (ac-nihongo-get-candidates-1 prefix buf)))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  (cl-loop for cands in (gethash (substring-no-properties prefix
                                                          0
                                                          ac-nihongo--hashtable-key-length)
                                 (ac-nihongo-get-hashtable buf))
           when (string-match-p (format "^%s" prefix) cands)
           collect cands into candidates
           finally return (sort candidates #'string<)))

(defun ac-nihongo-get-hashtable (buf)
  (unless (assoc buf ac-nihongo--index-cache-alist)
    (cl-loop with table = (make-hash-table :test #'equal)
             for word in (with-current-buffer buf
                           (ac-nihongo-split-string
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
             when (> (length word) 1)
             do (puthash (substring word 0 ac-nihongo--hashtable-key-length) word table)
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

(defun ac-nihongo-prefix ()
  )

(ac-define-source
  '((candidates . ac-nihongo-get-candidates)
    (prefix . ac-nihongo-prefix)
    (requires . 0)))
