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

;;; Code:

(require 'cl-lib)
(require 'auto-complete)

;;; Customization

(defcustom ac-nihongo-separator-regexp "[[:ascii:]、。・「」：？…（）\n\t]"
  "Default separator to split string in buffers. Characters in
  this regexp are excluded from candidates."
  :type 'regexp
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-limit 30
  "The upper number of candidates to show when completing."
  :type 'number
  :group 'auto-complete-nihongo)

(defcustom ac-nihongo-select-buffer-function #'ac-nihongo-select-exact-same-mode-buffers
  "A function that returns a list of buffers. Those buffers are
  searched for candidates."
  :type 'symbol
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

(defun ac-nihongo-select-exact-same-mode-buffers ()
  "Return buffers that have the same major mode as the current bufer
does."
  (cl-remove-if-not
   (lambda (buf) (eq major-mode (buffer-local-value 'major-mode buf)))
   (buffer-list)))

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
  "Return a list of candidates that begin with PREFIX by
searching in current buffer."
  (let ((limit (or (and (integerp ac-limit) ac-limit)
                   (and (integerp ac-nihongo-limit) ac-nihongo-limit)
                   10))
        (cnt 0)
        (regexp (format "\\(%s%s+\\)" prefix (ac-nihongo-get-regexp)))
        (cand nil)
        (table (make-hash-table :test #'equal))
        (pos (point))
        (candidates nil))
    (save-excursion
      (ignore-errors (goto-char (1- pos)))
      ;; search backward
      (while (and (< cnt limit)
                  (re-search-backward regexp nil t))
        (setq cand (match-string-no-properties 0))
        (unless (gethash cand table)
          (puthash cand t table)
          (cl-incf cnt)))
      (ignore-errors (goto-char (1+ pos)))
      ;; then search forward
      (while (and (< cnt limit)
                  (re-search-forward regexp nil t))
        (setq cand (match-string-no-properties 0))
        (unless (gethash cand table)
          (puthash cand t table)
          (cl-incf cnt))))
    (maphash (lambda (k v) (push k candidates)) table)
    candidates))

(defun ac-nihongo-get-candidates-1 (prefix buf)
  "Return a list of candidates that begin with PREFIX in buffer BUF."
  (cond
   ((eq buf (current-buffer))
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
  "Return a hashtable that holds nihongo words in buffer BUF.

If a hashtable has not been created for buffer BUF, create a new one,
and put words in buffer BUF into this table, and store it in
`ac-nihongo--index-cache-alist'."
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
             when (and (> (length word) 1) (not (gethash word inserted-words)))
             if (gethash key table)
             do (progn (puthash word t inserted-words)
                       ;; this word has not been inserted yet
                       (push word (gethash key table)))
             else
             do (progn (puthash key (list word) table)
                       (puthash word t inserted-words))
             finally (push (cons buf table) ac-nihongo--index-cache-alist)))
  (assoc-default buf ac-nihongo--index-cache-alist))

(defun ac-nihongo-split-string (s)
  "Extract nihongo words by splitting string S by
`ac-nihongo-separator-regexp'."
  (delete-dups
   (nconc
    ;; Extract 2-byte hiragana words. Katakana and Kanji are also used
    ;; as separator. Here, we are saying "word", but it actually a
    ;; string consisting of hiragana characters. Same goes for
    ;; katakana words and kanji words.
    (with-category-table ac-nihongo--hiragana-category-table
      (split-string s (concat ac-nihongo-separator-regexp "\\|\\cK\\|\\cC") t))
    ;; Extract 2-byte katakana words
    (with-category-table ac-nihongo--katakana-category-table
      (split-string s (concat ac-nihongo-separator-regexp "\\|\\cH\\|\\cC") t))
    ;; Extract kanji words
    (split-string s (concat ac-nihongo-separator-regexp "\\|\\cH\\|\\cK") t))))

(defun ac-nihongo-get-regexp ()
  "Return regexp to be used to determine prefix depending on the type
of `char-before'."
  (let ((ch (char-to-string (char-before))))
    ;; Bug: When ch is "～", both hiraganra and katakana will be
    ;; correct. How do we decide which regexp we use?
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
  "Debugging purpose."
  (interactive)
  (setq ac-sources '(ac-source-nihongo)))

(ac-define-source nihongo
  '((candidates . ac-nihongo-get-candidates)
    (prefix . ac-nihongo-prefix)
    (requires . 0)))

(provide 'auto-complete-nihongo)
