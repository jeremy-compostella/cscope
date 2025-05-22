;;; cscope.el --- Client Interface to cscope. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: April 2025
;; Keywords: extensions cscope
;; Homepage: https://github.com/jeremy-compostella/cscope
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (magit "3.3.0"))

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

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-seq))
(require 'grep)
(require 'magit)

(defconst cscope-mode-line-matches
  `(" [" (:propertize (:eval (int-to-string cscope-num-matches-found))
                      face ,grep-hit-face
                      help-echo "Number of matches so far")
    "]"))

(defvar cscope-history '()
  "History list for symbols queried by cscope search functions.")

(defcustom cscope-search-types
  '((0 . "find-this-symbol")
    (1 . "find-global-definition")
    (2 . "find-called-functions")
    (3 . "find-functions-calling-this-function")
    (4 . "find-this-text-string")
    (8 . "find-files-including-file"))
  "Alist mapping cscope search type numbers to their descriptive
names and documentation."
  :type 'alist)

(defvar-local cscope-process nil
  "Process object for the running cscope command.")

(defvar-local cscope-searches nil
  "A list of previous searches performed in this buffer.
Each element is a cons cell `(type . symbol)`.")

(defvar-local cscope-searches-backup nil)

(defvar-local cscope-leftover nil
  "Holds any incomplete line received from the cscope process.")

(defvar-local cscope-num-matches-found 0
  "Number of matches found so far in this buffer for the current search.")

(defun cscope-start-process ()
  "Start the cscope process in line-oriented mode.
Uses the database file 'cscope.out' in the current default directory.
Sets the `cscope-process' variable and assigns `cscope-filter' as
the process filter function."
  (setq cscope-process
	(start-file-process "cscope" (current-buffer) "cscope" "-ld" "-f"
			    "cscope.out"))
  (set-process-filter cscope-process #'cscope-filter))

(defun cscope-database-sentinel (progress timer process string)
  "Sentinel function for the cscope database generation process.
This function is called when the process started by
`cscope-generate-database' finishes. It checks the process output
to determine if the database generation was successful.

If successful, it starts the main cscope query process using
`cscope-start-process' and, if there are pending searches in
`cscope-searches', executes the most recent one using
`cscope-execute-query'.

If the database generation failed, it signals an error.

PROGRESS is the `progress-reporter' object used during generation.
TIMER is the timer object used to update the progress reporter.
PROCESS is the process object that finished.
STRING is the output from the process."
  (with-current-buffer (process-buffer process)
    (cancel-timer timer)
    (progress-reporter-done progress)
    (if (not (string= string "finished\n"))
	(error "Database generation failed, check %s buffer for details"
	       (buffer-name))
      (cscope-start-process)
      (when cscope-searches
	(cscope-execute-query)))))

(defun cscope-generate-database-command ()
  "Generate the command string to create the cscope database.

This function checks for the presence of a specific header file,
'include/linux/linux_logo.h', to determine if this is a Linux
kernel repository that provides a cscope make target. Otherwise,
a find command is used to create the list of files before running
cscope."
  (if (file-readable-p "include/linux/linux_logo.h")
      "make cscope"
    (let ((files "find . \\( -name '*.[ch]' -o -name '*.cpp' \\) > cscope.files")
	  (database "cscope -b -q"))
      (concat files "&&" database))))

(defun cscope-generate-database ()
  "Generate the cscope database in the current directory.
This function creates a 'cscope.files' file containing a list of C,
C++, and header files in the current directory and its subdirectories
using 'find'. It then builds the cscope database 'cscope.out' using
'cscope -b -q' based on this file.

The process is run asynchronously using `start-file-process-shell-command'.
A `progress-reporter' is used to show the generation status in the echo area.
The `cscope-database-sentinel' function is set as the process sentinel
to handle completion."
  (interactive)
  (let* ((process (start-file-process-shell-command
		   "cscope" (current-buffer) (cscope-generate-database-command)))
	 (progress (make-progress-reporter
		    (format "Generating cscsope database for %s..."
			    default-directory)))
	 (timer-func (lexical-let ((progress progress))
		       (apply-partially #'progress-reporter-update progress)))
	 (timer (run-at-time .5 2 timer-func)))
    (set-process-sentinel
     process (apply-partially #'cscope-database-sentinel progress timer))))

(defun cscope-buffers ()
  "Return a list of all buffers currently in `cscope-mode'."
  (seq-filter (lambda (b)
		(with-current-buffer b
		  (eq major-mode 'cscope-mode)))
	      (buffer-list)))

(defun cscope-new-buffer (dir)
  "Generate a new buffer for cscope results associated with DIR.
The buffer name is based on the non-directory part of DIR,
prefixed with '*cscope:' and suffixed with '*'."
  (when (string= (substring dir -1)  "/")
    (setf dir (substring dir 0 -1)))
  (let ((buffer (generate-new-buffer
		 (format "*cscope:%s*" (file-name-nondirectory dir)))))
    (with-current-buffer buffer
      (setq default-directory dir
            buffer-read-only nil)
      (cscope-mode)
      (current-buffer))))

(defun cscope-top-level (dir)
  "Determine the top-level directory for cscope operations for DIR."
  (cond ((magit-toplevel))
	((let ((top-level)
	       (current dir))
	   (while (and current (not top-level))
	     (let ((default-directory current))
	       (when (file-readable-p "cscope.out")
		 (setf top-level current)))
	     (setf current (file-name-parent-directory current)))
	   top-level))
	(dir)))

(defun cscope-find-buffer (dir)
  "Find or create a cscope buffer for the given directory DIR.
Returns an existing `cscope-mode' buffer if its `default-directory'
is a prefix of DIR. Otherwise, creates a new `cscope-mode' buffer
with its `default-directory' set to the project's top-level directory
(if using git) or DIR, and returns it."
  (or (cl-find (expand-file-name dir) (cscope-buffers)
               :key (lambda (b)
                      (with-current-buffer b default-directory))
               :test (lambda (x y)
		       (string-prefix-p (expand-file-name y) x)))
      (cscope-new-buffer (cscope-top-level dir))))

(defun cscope-backup-incomplete-line (buffer)
  "Store the content of the current line in BUFFER's `cscope-leftover'.
This is called by the filter function when the cscope process
output ends mid-line, so the partial line can be processed later
when more output arrives."
  (let ((incomplete (buffer-substring (line-beginning-position)
				      (line-end-position))))
    (with-current-buffer buffer
      (setq-local cscope-leftover incomplete))))

(defun cscope-restore-incomplete-line (buffer)
  "Restore the content of `cscope-leftover' in BUFFER to the current buffer.
This is called by the filter function after receiving more output
from the cscope process to prepend the previously incomplete line
to the new output chunk for correct parsing."
  (when-let ((leftover (with-current-buffer buffer
			 (prog1
			     cscope-leftover
			   (setq-local cscope-leftover nil)))))
    (insert leftover)))

(defun cscope-insert-result (buffer file function line context)
  "Insert a cscope search result into BUFFER.

The result is formatted as 'file:line: context\\n'.
Increments `cscope-num-matches-found'.
If this is the second match found, displays the buffer.
Highlights the search symbol in the context."
  (with-current-buffer buffer
    (cl-incf cscope-num-matches-found)
    (when (= cscope-num-matches-found 2)
      (display-buffer (current-buffer)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (format "%s:%s:" file line))
        (let ((start (point)))
          (insert (string-trim-left context) "\n")
          (save-excursion
            (goto-char start)
            (when (search-forward (cdar cscope-searches) nil t)
              (replace-match (propertize (match-string 0)
                                         'face nil
                                         'font-lock-face 'match)
                             t t))))))))

(defun cscope-filter (process output)
  "Filter the output from the cscope process.
Parses the output from the cscope process, extracts file, function,
line number, and context, and inserts the results into the cscope
buffer. Handles incomplete lines and updates the mode line to
indicate the status of the search."
  (let ((buffer (process-buffer process)))
    (with-temp-buffer
      (cscope-restore-incomplete-line buffer)
      (insert output)
      (goto-char (point-min))
      ;; Parse results
      (let* ((rpath "\\([a-DA-Z0-9_<>/\.]+\\)")
	     (rfunction "\\([a-DA-Z0-9_<>]+\\)")
	     (rnumber "\\([0-9]+\\)")
	     (regexp (format "^%s %s %s \\(.*\\)$" rpath rfunction rnumber)))
	(while (re-search-forward regexp nil t)
	  (if (= (point-max) (line-end-position))
	      (cscope-backup-incomplete-line buffer)
	    (let ((file (match-string 1))
		  (function (match-string  2))
		  (line (string-to-number (match-string 3)))
		  (context (match-string 4)))
	      (cscope-insert-result buffer
				    (match-string 1) (match-string 2)
				    (match-string 3) (match-string 4)))))
	(unless (= (point) (point-max))
	  (forward-char))
	(when (and (= (point) (line-beginning-position))
		   (not (= (point) (line-end-position))))
	  (cscope-backup-incomplete-line buffer)))
      ;; End of data
      (when (re-search-forward "^>>" nil t)
	(with-current-buffer buffer
	  (let ((face (if (= cscope-num-matches-found 0)
			  'compilation-mode-line-fail
			'compilation-mode-line-exit)))
	    (setq mode-line-process
		  `((:propertize ":exit" face ,face)
                    compilation-mode-line-errors)))
	  (if (= cscope-num-matches-found 0)
	      (message "No match found for '%s'." (cdar cscope-searches))
	    (goto-line 3)
	    (when (= cscope-num-matches-found 1)
	      (next-error))))))))

(defun cscope-type-title (type)
  "Convert a cscope search type (a string with dashes) into a title.
For example, 'find-this-symbol' becomes 'Find this symbol'."
  (with-temp-buffer
    (save-excursion
      (insert type))
    (capitalize-word 1)
    (while (search-forward "-" nil t)
      (replace-match " "))
    (buffer-string)))

(defun cscope-search-message (search)
  "Format a search query for display.
Takes a SEARCH query, which is a cons cell `(type . symbol)`, and
generates a human-readable string describing the search."
  (format "%s: '%s'."
	  (cscope-type-title (assoc-default (car search) cscope-search-types))
	  (cdr search)))

(defun cscope-execute-query ()
  "Execute the most recent cscope query.

This function takes the most recent search query from `cscope-searches'
and sends it to the running cscope process.  It first prepares the
cscope buffer by clearing it and displaying the search query."
  (let ((search (car cscope-searches)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (cscope-search-message search) "\n\n"))
    (setq mode-line-process
	  '((:propertize ":run" face compilation-mode-line-run)
            compilation-mode-line-errors)
	  cscope-num-matches-found 0)
    (process-send-string cscope-process (format "%d%s\n" (car search)
						(cdr search)))))

(defun cscope-read-string (prompt)
  "Read a string from the minibuffer, adding it to `cscope-history'."
  (let ((initial (cond ((use-region-p)
			(buffer-substring-no-properties (region-beginning)
							(region-end)))
		       ((thing-at-point 'symbol)))))
    (read-string type initial 'cscope-history)))

(defun cscope-query (&optional type thing)
  "Initiate a cscope search of TYPE for THING."
  (interactive)
  (unless type
    (setf type (completing-read
		"Type: " (mapcar 'cdr cscope-search-types) nil t)))
  (unless thing
    (setf thing
	  (cscope-read-string (concat (cscope-type-title type) ": "))))
  (when (stringp type)
    (setf type (car (rassoc type cscope-search-types))))
  (with-current-buffer (cscope-find-buffer default-directory)
    (let ((search (cons type thing)))
      (setq cscope-searches (delete search cscope-searches)
	    cscope-searches (push search cscope-searches))
      (unless (and cscope-process (process-live-p cscope-process))
	(if (file-readable-p "cscope.out")
	    (cscope-start-process)
	  (cscope-generate-database)))
      (when (and cscope-process (process-live-p cscope-process))
	(cscope-execute-query)))))

(defun cscope-previous-query (&optional n)
  "Execute a previous cscope query from the history.

With a positive prefix argument N, executes the Nth previous query.
With a negative prefix argument N, executes the Nth next query (undoing
previous queries).  Without a prefix argument, executes the immediately
previous query.

The history is maintained in `cscope-searches' and
`cscope-searches-backup'.  Queries are moved between these lists
to navigate the history."
  (interactive)
  (let* ((i 0)
	 (n (or n 1)))
    (let ((in (if (< n 0) 'cscope-searches-backup 'cscope-searches))
	  (out (if (< n 0) 'cscope-searches 'cscope-searches-backup)))
      (while (and (< i (abs n)) (cdr (symbol-value in)))
	(set out (push (car (symbol-value in)) (symbol-value out)))
	(set in (cdr (symbol-value in)))
	(cl-incf i)))
    (when (= i (abs n))
      (cscope-execute-query))))

(defun cscope-next-query (&optional n)
  "Execute a subsequent cscope query from the history (undoing previous).

With a positive prefix argument N, executes the Nth next query (undoing
previous queries). Without a prefix argument, executes the immediately
next query.

This function is essentially a wrapper around `cscope-previous-query'
with a negated argument."
  (interactive)
  (let ((n (or n 1)))
    (cscope-previous-query (* -1 n))))

(dolist (feature cscope-search-types)
  (fset (intern (concat "cscope-" (cdr feature)))
	(lexical-let ((feature feature))
	  (lambda (symbol)
	    (interactive
	     (list (read-string "Symbol: " (current-word)
				'cscope-history)))
	    (cscope-query (car feature) symbol)))))

(defvar cscope-mode-map grep-mode-map)

;;;###autoload
(define-compilation-mode cscope-mode "Cscope"
  "Major mode for displaying cscope search results.

This mode is derived from `compilation-mode`, providing features
like `next-error` and `previous-error` to navigate through search
results. The mode line displays the number of matches found."
  (setq-local compilation-error-regexp-alist grep-regexp-alist
	      compilation-error-face grep-hit-face
	      compilation-mode-line-errors cscope-mode-line-matches))

(provide 'cscope)
