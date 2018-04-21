;;; auto-complete-nihongo.el --- Source for auto-complete.el.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL:
;; Version: 0.01
;; Package-Requires: ((auto-complete) (cl))
;; Keywords: auto-comlete

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ;; Thanks to http://d.hatena.ne.jp/khiker/20100427
;; (defun my-popup-isearch-regexp-migemo-function (pattern)
;;   (concat (regexp-quote pattern)
;;           ;; migemoize. (require 'migemo) is required!
;;           (and pattern
;;                (fboundp 'migemo-get-pattern)
;;                (concat "\\|"
;;                        (migemo-get-pattern pattern)))))
;;
;; (setq popup-isearch-regexp-builder-function #'my-popup-isearch-regexp-migemo-function)
;;
;; (defun my-ac-isearch-around-ad-func (orig-fun &rest args)
;;   (let ((state (ime-get-mode)))
;;     ;; Disable IME if need be.
;;     (and state (toggle-input-method))
;;     (apply orig-fun args)
;;     ;; Enable IME if need be.
;;     (and state (toggle-input-method))))
;;
;; (when (fboundp 'ime-get-mode)
;;   (advice-add 'ac-isearch :around #'my-ac-isearch-around-ad-func))
;; ;; (advice-remove 'ac-isearch #'my-ac-isearch-around-ad-func)
;;
;; (when (featurep 'skk)
;;   (add-to-list 'ac-trigger-commands 'skk-insert)
;;   (add-to-list 'ac-trigger-commands 'skk-kakutei))

;;; Code:

(require 'cl-lib)
(require 'auto-complete)


;;; Customization

(defcustom ac-nihongo-separator-regexp "[[:ascii:]、。・「」：？…（）\n\t]"
  "Default separator to split string in buffers. Characters in
  this regexp are excluded from candidates."
  :type 'regexp
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-limit 20
  "The upper number of candidates to show when completing."
  :type 'number
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-select-buffer-function #'ac-nihongo-select-target-mode-buffers
  "A function that returns a list of buffers. Those buffers are
  searched for candidates."
  :type 'symbol
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-mode-group-list
  '((emacs-lisp-mode lisp-interaction-mode)
    (python-mode inferior-python-mode))
  ""
  :type 'list
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-ascii-regexp "[0-9A-Za-z_-]"
  "Regexp used to search for candidates that are not multibyte strings."
  :type 'regexp
  :group 'auto-complete-nihongo)

;;; Variables

(defvar ac-nihongo--index-cache-alist nil
  "Alist in the form of (buffer . hashtable).")

(defvar ac-nihongo--hashtable-key-length 1
  "The length of key to hash table.")

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
  "Category table used to extract 2-byte hiragana words.")

(defvar ac-nihongo--katakana-category-table (ac-nihongo--make-category-table ?H)
  "Category table used to extract 2-byte katakana words.")

(defvar ac-nihongo--not-found-prefix nil
  "Prefix string used in the last search, which failed to find any
candidate in current buffer. `nil' means that the last search found
something.")

(defvar ac-nihongo--not-found-regexp nil
  "Regexp used in the last search which failed ot find candidates.")

(defvar ac-nihongo--not-found-buffer nil
  "The buffer in which the last search was performed and it failed to
find candidates.")

(defun ac-nihongo-select-target-mode-buffers ()
  "Return buffers that have the same major mode as that of current
buffer."
  (let ((target-major-modes (or (cl-find-if
                                 (lambda (elt)
                                   (memq major-mode elt))
                                 ac-nihongo-mode-group-list)
                                (list major-mode))))
    (cl-remove-if-not (lambda (buf)
                        (memq (buffer-local-value 'major-mode buf)
                              target-major-modes))
                      (buffer-list))))

(defun ac-nihongo-get-candidates ()
  "Return a list of candidates that begin with `ac-prefix'."
  (when ac-prefix
    (delete-dups
     (sort (cl-loop for buf in (funcall ac-nihongo-select-buffer-function)
                    with limit = (or ac-nihongo-limit
                                     (and (integerp ac-limit)
                                          ac-limit))
                    nconc (ac-nihongo-get-candidates-1 ac-prefix buf) into candidates
                    when (and (integerp limit)
                              (> (length candidates) ac-nihongo-limit))
                    return candidates
                    finally return candidates)
           #'string<))))

(defun ac-nihongo-get-candidates-in-current-buffer (prefix)
  "Return a list of candidates in current buffer that begin with
PREFIX."
  (let* ((limit (or (and (integerp ac-limit) ac-limit)
                    (and (integerp ac-nihongo-limit) ac-nihongo-limit)
                    10))
         (cnt 0)
         (cell (ac-nihongo--make-regexp prefix))
         (prefix-regexp (and (consp cell) (car cell)))
         (cand-regexp (and (consp cell) (cdr cell)))
         (cand nil)
         (table (make-hash-table :test #'equal))
         (pos (point))
         (candidates nil)
         (tail-candidates nil)
         (prefix-len (length prefix)))
    (cl-assert (and prefix-regexp cand-regexp))
    (when (ac-nihongo--go-search-p prefix prefix-regexp (current-buffer))
      ;; Note: Would it better to use hashtable for collecting candidates
      ;; in current buffer?
      ;; (message "DEBUG: in-current-buffer, prefix=%s, ac-nihongo--not-found-prefix=%s" prefix ac-nihongo--not-found-prefix)
      (ac-nihongo-search-candidates-in-buffer cand-regexp
                                              prefix-len
                                              (1- pos)
                                              limit
                                              table
                                              #'re-search-backward)
      (ac-nihongo-search-candidates-in-buffer cand-regexp
                                              prefix-len
                                              (1+ pos)
                                              limit
                                              table
                                              #'re-search-forward)
      ;; Collect candidates in table
      (maphash (lambda (cand v)
                 (push cand candidates)
                 (when (string-match (format "\\(%s\\).*" prefix-regexp) cand)
                   ;; cand contains different characters other than
                   ;; prefix-regexp-matching ones. Extract
                   ;; prefix-regexp-matching string part and put it
                   ;; into hash table.
                   ;; If prefix is "Vi" and cand is "Vimプロフェッショナル",
                   ;; for example, then we also want to collect "Vim"
                   ;; as a candidate.
                   (push (match-string-no-properties 1 cand) tail-candidates))
                 (when (and (string-match-p ac-nihongo-ascii-regexp cand)
                            (setq lst (split-string cand "[_-]" t)))
                   ;; If cand is "abc-def", we make "def" a candidate as well.
                   (mapc (lambda (elt) (push elt tail-candidates)) lst)))
               table)
      (if candidates
          ;; Found candidates and we clear not-found-state.
          (ac-nihongo--clear-not-found-state)
        ;; Found nothing and we set not-found-state
        (ac-nihongo--set-not-found-state prefix prefix-regexp (current-buffer)))
      (append candidates tail-candidates))))

(defun ac-nihongo-search-candidates-in-buffer (regexp min-len pos limit table search-func)
  (let ((cand nil))
    (save-excursion (ignore-errors (goto-char pos)))
    (while (and (< (hash-table-count table) limit)
                (funcall search-func regexp nil t))
      (setq cand (match-string-no-properties 0))
      (when (< min-len (length cand))
        (puthash cand t table)))))

(defun ac-nihongo--go-search-p (prefix prefix-regexp buffer)
  (cond
   ((or (null ac-nihongo--not-found-buffer)
        (not (eq ac-nihongo--not-found-buffer buffer)))
    ;; Must have switched to another buffer and we have to search in BUFFER.
    t)
   ((and (null ac-nihongo--not-found-prefix)
         (null ac-nihongo--not-found-regexp))
    t)
   ((and (stringp ac-nihongo--not-found-prefix)
         (eq ac-nihongo--not-found-buffer buffer))
    ;; The last search was done in buffer
    ;; `ac-nihongo--not-found-buffer' with prefix being
    ;; `ac-nihongo--not-found-buffer' and no candidates are found.
    ;; Therefore, if `ac-nihongo--not-found-prefix' is still a prefix
    ;; of PREFIX, we don't need to do search.
    (not (string-match-p (concat "^" ac-nihongo--not-found-prefix) prefix)))
   (t
    t)))

(defun ac-nihongo--clear-not-found-state ()
  (setq ac-nihongo--not-found-buffer nil)
  (setq ac-nihongo--not-found-prefix nil)
  (setq ac-nihongo--not-found-regexp nil))

(defun ac-nihongo--set-not-found-state (prefix prefix-regexp buffer)
  (setq ac-nihongo--not-found-buffer buffer)
  (setq ac-nihongo--not-found-prefix prefix)
  (setq ac-nihongo--not-found-regexp prefix-regexp))

(defun ac-nihongo--make-regexp (prefix)
  "Make regexp to be used in
`ac-nihongo-get-candidates-in-current-buffer' and returns a cons cell,
whose car is a regexp that represents prefix, and cdr is also a regexp
used to search for candidates."
  (cond
   ((string-match-p (format "^%s+$" ac-nihongo-ascii-regexp) prefix)
    ;; "ascii" or "ascii + katakana"
    (cons (format "%s+" ac-nihongo-ascii-regexp)
          (format "%s%s*\\cK*" prefix ac-nihongo-ascii-regexp)))
   ((string-match-p "^\\cH+$" prefix)
    ;; "hiragana" or "hiragan + kanji"
    (cons "\\cH+" (format "%s\\cH*\\cC*" prefix)))
   ((string-match-p "^\\cK+$" prefix)
    ;; "katakana" or "katakana + hiragana"
    (cons "\\cK+" (format "%s\\cK*\\cH*" prefix)))
   ((string-match-p "^\\cC+$" prefix)
    ;; "kanji" or "kanji + hiragana"
    (cons "\\cC+" (format "%s\\cC*\\cH+" prefix)))
   (t
    nil)))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  "Return a list of candidates that begin with PREFIX in buffer BUF."
  (cond
   ((eq buf (current-buffer))
    ;; Reset hashtable for current buffer.
    (and (assoc buf ac-nihongo--index-cache-alist)
         (assq-delete-all buf ac-nihongo--index-cache-alist))
    (ac-nihongo-get-candidates-in-current-buffer prefix))
   (t
    (ac-nihongo-get-candidates-in-other-buffer prefix buf))))

(defun ac-nihongo-get-candidates-in-other-buffer (prefix buf)
  (cl-loop for cand in (gethash (substring-no-properties prefix
                                                         0
                                                         ac-nihongo--hashtable-key-length)
                                (ac-nihongo-get-hashtable buf))
           ;; gethash above returns a sorted list of candidates.
           ;; So the second time `string-match-p' below returns nil,
           ;; and this means that there is no more candidates that
           ;; will match with PREFIX.
           ;; (aabc abc b bb bba bbb bbc c ca ...)
           ;;           ^                ^
           ;;           |                |
           ;;        found = t       string-match-p returns nil
           ;; prefix="b"
           with found = nil
           with candidates = nil
           if (string-match-p (format "^%s.*" prefix) cand)
           do (progn (or found (setq found t))
                     (push cand candidates))
           else if found
           ;; string-match-p returns nil, but we have already found
           ;; candidates and we just passed over candidates. No need
           ;; to search for more.
           return candidates
           finally return candidates))

(defun ac-nihongo-get-hashtable (buf &optional new)
  "Return a hashtable that holds words in buffer BUF.

If a hashtable has not been created for buffer BUF, or argument NEW is
non-nil, create a new one, and put words in buffer BUF into this
table, and store it in `ac-nihongo--index-cache-alist'."
  (when (or (null (assoc buf ac-nihongo--index-cache-alist)) new)
    ;; Make a new hashtable for this buffer. Key is a string of length
    ;; ac-nihongo--hashtable-key-length and its value is a list of
    ;; strings, sorted.
    ;; i.e. "あ" => '("あい" "あお" "あほ" "あんこ" ...)
    ;;      "p"  => '("pop" "prin1" "prin1-to-string" "push" ...)
    (ac-nihongo--register-hashtable buf))
  (assoc-default buf ac-nihongo--index-cache-alist))

(defun ac-nihongo--register-hashtable (buffer)
  (cl-loop with table = (make-hash-table :test #'equal)
           with inserted-words = (make-hash-table :test #'equal)
           with res-table = (make-hash-table :test #'equal)
           for word in (ac-nihongo--get-word-list buffer)
           for key = (substring word 0 1)
           when (and (> (length word) 1) (not (gethash word inserted-words)))
           if (gethash key table)
           do (progn (puthash word t inserted-words)
                     ;; this word has not been inserted yet
                     (push word (gethash key table)))
           else
           do (progn (puthash key (list word) table)
                     (puthash word t inserted-words))
           finally (progn (maphash (lambda (k v)
                                     ;; sort each list
                                     (puthash k (sort v #'string<) res-table))
                                   table)
                          (push (cons buffer res-table) ac-nihongo--index-cache-alist))))

(defun ac-nihongo--get-word-list (buffer)
  "Split buffer string by the type of character and return a list of
would-be candidates."
  (cl-loop with lst = (ac-nihongo-split-buffer-string buffer)
           with ret = nil
           for curr in lst
           for next in (cdr lst)
           do (progn (push curr ret)
                     (when (ac-nihongo--is-target-word curr next)
                       (push (concat curr next) ret)))
           finally return ret))

(defun ac-nihongo--is-target-word (curr next)
  (or (and next
           ;; "kanji + hiragana" and
           ;; "katakana + hiragana"
           (string-match-p "\\cC+\\|\\cK+" curr)
           (string-match-p "\\cH+" next))
      (and next
           ;; "alphabet" + "katakana"
           (string-match-p (format "%s+" ac-nihongo-ascii-regexp) curr)
           (string-match-p "\\cK+" next))
      (and next
           ;; "katakana" + "hiragana"
           (string-match-p "\\cK+" curr)
           (string-match-p "\\cH+" next))
      (and next
           ;; "hiragana" + "kanji"
           (string-match-p "\\cH+" curr)
           (string-match-p "\\cK+" next))))

(defun ac-nihongo-split-buffer-string (buffer)
  "Return a list of hiragana words, katakana words and kanji words in
current buffer."
  (let ((ret nil)
        (regexp (format "\\cH+\\|\\cK+\\|\\cC+\\|%s+" ac-nihongo-ascii-regexp))
        (word nil))
    (with-current-buffer buffer
      (save-excursion (goto-char (point-min))
                      (while (re-search-forward regexp nil t)
                        (setq word (match-string-no-properties 0))
                        (push word ret)
                        (when (string-match-p ac-nihongo-ascii-regexp word)
                          ;; If word is like "abc-def", then we push
                          ;; abc and def into ret as well.
                          (mapc (lambda (elt) (push elt ret))
                                (split-string word "[_-]" t))))))
    (nreverse ret)))

(defun ac-nihongo-get-regexp ()
  "Return regexp to be used to determine prefix depending on the type
of `char-before'."
  (let* ((ch (char-to-string (char-before)))
         (anomaly-characters "ー〜")
         (anomaly-regexp (format "[%s]" anomaly-characters)))
    (cond
     ;; ascii-word constituent characters
     ((string-match-p (format "%s" ac-nihongo-ascii-regexp) ch)
      ac-nihongo-ascii-regexp)
     ((string-match-p anomaly-regexp ch)
      ;; Special case:
      ;; When ch is "～" or "ー", both hiraganra and katakana will be
      ;; correct as regexp, so we will look back for another
      ;; character.
      (cond
       ((save-excursion
          (and (re-search-backward (format "[^%s]" anomaly-characters) nil t)
               (setq ch (char-to-string (char-after)))))
        ;; We found a character other than "ー" or "〜".
        (ac-nihongo-get-regexp-1 ch))
       (t
        ;; Return hiragana as a fallback case.
        "\\cH")))
     (t
      (ac-nihongo-get-regexp-1 ch)))))

(defun ac-nihongo-get-regexp-1 (ch)
  (cond
   ;; 2-byte hiragana
   ((string-match-p "\\cH" ch) "\\cH")
   ;; 2-byte katakana
   ((string-match-p "\\cK" ch) "\\cK")
   ;; 2-byte kanji
   ((string-match-p "\\cC" ch) "\\cC")
   (t nil)))

(defun ac-nihongo-prefix ()
  (let ((regexp (ac-nihongo-get-regexp)))
    (unless regexp
      ;; Clear the last search state.
      (ac-nihongo--clear-not-found-state))
    (when regexp
      (save-excursion
        (backward-char)
        (while (and (not (bobp))
                    (looking-at-p regexp))
          (backward-char))
        (if (looking-at-p regexp)
            (point)
          (1+ (point)))))))

(defun ac-nihongo-skk-kakute-maybe ()
  "When using skk and in `skk-henkan-mode', get `skk-kakutei' be
called on completion."
  (when (and (featurep 'skk)
             skk-henkan-mode)
    (call-interactively #'skk-kakutei)))

(defun ac-nihongo-init ()
  "Debugging purpose."
  (interactive)
  (setq ac-nihongo--index-cache-alist nil)
  (setq ac-nihongo--not-found-prefix nil
        ac-nihongo--not-found-regexp nil
        ac-nihongo--not-found-buffer nil)
  (setq ac-sources '(ac-source-nihongo)))

(defun ac-nihongo-toggle-source ()
  (interactive)
  (if (memq 'ac-source-nihongo ac-sources)
      (setq ac-sources (delq 'ac-source-nihongo ac-sources))
    (push 'ac-source-nihongo ac-sources)))

(ac-define-source nihongo
  '((candidates . ac-nihongo-get-candidates)
    (prefix . ac-nihongo-prefix)
    (requires . 0)
    (action . ac-nihongo-skk-kakute-maybe)))

(provide 'auto-complete-nihongo)
