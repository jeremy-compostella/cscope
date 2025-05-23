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

(require 'cl)
(require 'cl-lib)
(require 'cl-seq)
(require 'grep)
(require 'magit)
(require 'uniquify)

(defcustom cscope-show-function t
  "Whether to show function name in results."
  :type 'boolean)

(defcustom cscope-fontify-code-line t
  "Whether to apply syntax highlighting to code lines in results."
  :type 'boolean)

(defcustom cscope-highlight-and-font-line-limit 500
  "The maximum number of lines to apply syntax highlighting in search results.

When the number of lines exceeds this limit, syntax highlighting will be
disabled to improve performance. This helps prevent slowdowns when dealing
with large result sets."
  :type 'number)

(defcustom cscope-highlight-match t
  "When non-nil, highlights the matching symbols in search results."
  :type 'boolean)

(defcustom cscope-search-types
  '((0 "find-this-C-symbol" ",")
    (1 "find-this-function-definition" ".")
    (2 "Find-functions-called-by-this-function" "c")
    (3 "find-functions-calling-this-function" "C")
    (4 "find-this-text-string" "s")
    (5 "change-this-text-string" "S")
    (6 "find-this-egrep-pattern" "e")
    (7 "find-this-file" "f")
    (8 "find-files-including-file" "i")
    (9 "find-assignments-to-this-symbol" "a"))
  "Alist mapping cscope search type numbers to their descriptive
names and documentation."
  :type 'alist)

(defcustom cscope-display-options
  '(("t" . truncate-lines)
    ("s" . cscope-show-function)
    ("f" . cscope-fontify-code-line)
    ("h" . cscope-highlight-match))
  "Alist controlling display options in the cscope buffer.
Each element is a list of the form (VARIABLE KEY), where:
- VARIABLE is the symbol of the boolean variable controlling the option.
- KEY is the character used to toggle the option in the transient menu."
  :type 'alist)

(defun cscope-symbol-title (symbol)
  "Generate a user-friendly title from a symbol or symbol string.

This is used for displaying search types in menus and messages."
  (with-temp-buffer
    (save-excursion
      (insert (if (symbolp symbol)
		  (symbol-name symbol)
		symbol)))
    (save-excursion
      (when (search-forward "cscope-" nil t)
	(replace-match "")))
    (capitalize-word 1)
    (while (search-forward "-" nil t)
      (replace-match " "))
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar cscope-history '()
  "History list for symbols queried by cscope search functions.")

(defvar-local cscope-process nil
  "Process object for the running cscope command.")

(defvar-local cscope-searches nil
  "A list of previous searches performed in this buffer.
Each element is a cons cell (type . symbol).")

(defvar-local cscope-searches-backup nil)

(defvar-local cscope-leftover nil
  "Holds any incomplete line received from the cscope process.")

(defvar-local cscope-num-matches-found 0
  "Number of matches found so far in this buffer for the current search.")

(defvar-local cscope-inhibit-automatic-open nil
  "A flag to inhibit automatic opening of the first match.
If non-nil, prevents `cscope-filter' from automatically jumping
to the first match when a cscope query returns a single result.
This is useful when browsing and executing queries.")

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

If any root directory Makefile contains a reference to cscope, it
assumes there is a cscope make target."
  (if (= (shell-command "grep cscope Makefile*") 0)
      "make cscope"
    (let ((files "find . -name '*.[chsS]' > cscope.files")
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
  (with-current-buffer (cscope-find-buffer default-directory)
    (let* ((process (start-file-process-shell-command
		     "cscope" (current-buffer) (cscope-generate-database-command)))
	   (progress (make-progress-reporter
		      (format "Generating cscsope database for %s..."
			      default-directory)))
	   (timer-func (lexical-let ((progress progress))
			 (apply-partially #'progress-reporter-update progress)))
	   (timer (run-at-time .5 2 timer-func)))
      (set-process-sentinel
       process (apply-partially #'cscope-database-sentinel progress timer)))))

(defun cscope-buffers ()
  "Return a list of all buffers currently in `cscope-mode'."
  (seq-filter (lambda (b)
		(with-current-buffer b
		  (eq major-mode 'cscope-mode)))
	      (buffer-list)))

(defun cscope-new-buffer (dir)
  "Generate a new buffer for cscope results associated with DIR.
The buffer name is based on the last directory part of DIR,
prefixed with 'cscope:'."
  (when (string= (substring dir -1)  "/")
    (setf dir (substring dir 0 -1)))
  (let* ((directory (file-name-nondirectory (directory-file-name dir)))
	 (buf-name (format "cscope: %s" directory))
	 (buffer (generate-new-buffer buf-name)))
    (uniquify-rationalize-file-buffer-names buf-name directory buffer)
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

(defun cscope-fontify (context)
  "Apply syntax highlighting to CONTEXT string."
  (with-temp-buffer
    (insert context)
    (let ((prog-mode-hook '())
	  (c-mode-hook '()))
      (c-mode))
    (font-lock-ensure (point-min) (point-max))
    (goto-char (point-min))
    (let ((pos (point-min))
	  next)
      (while (setf next (next-property-change pos))
	(when-let ((face (get-text-property pos 'face)))
	  (put-text-property pos next 'face nil)
	  (put-text-property pos next 'font-lock-face face))
	(setf pos next)))
    (buffer-string)))

(defun cscope-insert-rendered-context (context)
  "Renders CONTEXT with optional fontification and highlighting.

This function takes a CONTEXT string, which represents a line of
code found by cscope, and applies optional transformations based
on customizable variables."
  (setf context (string-trim-left context))
  (when cscope-fontify-code-line
    (setf context (cscope-fontify context)))
  (save-excursion
    (insert context))
  (when cscope-highlight-match
    (let ((thing (cdar cscope-searches)))
      (while (search-forward thing (line-end-position) t)
	(let ((ol (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put ol 'face 'highlight)))))
  (goto-char (line-end-position)))

(defun cscope-insert-result (buffer file function line context)
  "Insert a cscope search result into BUFFER.

The result is formatted as 'file:line: context\\n'.
Increments `cscope-num-matches-found'.
If this is the second match found, displays the buffer.
Highlights the search symbol in the context."
  (with-current-buffer buffer
    (cl-incf cscope-num-matches-found)
    (when (= cscope-num-matches-found 1)
      (setq next-error-last-buffer buffer))
    (when (= cscope-num-matches-found 2)
      (display-buffer (current-buffer)))
    (when (and cscope-fontify-code-line
	       (= cscope-num-matches-found cscope-fontify-line-number-limit))
      (message "Fontification limit reached, disabling fontification."))
    (let* ((inhibit-read-only t)
	   (below-limit (< cscope-num-matches-found
			   cscope-highlight-and-font-line-limit))
	   (cscope-fontify-code-line (and below-limit
					  cscope-fontify-code-line))
	   (cscope-highlight-match (and below-limit
					cscope-highlight-match)))
      (save-excursion
        (goto-char (point-max))
	(let ((fun (if (or (string= function "<global>")
			   (string= function "<unknown>")
			   (string= function (cdar cscope-searches)))
		       ""
		     (propertize
		      (concat (propertize function 'face nil
					  'font-lock-face
					  'font-lock-function-name-face)
			      ":")
		      'invisible (not cscope-show-function)))))
          (insert (format "%s:%s:%s" file line fun))
	  (cscope-insert-rendered-context context)
	  (insert "\n"))))))

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
	    (if (and (not cscope-inhibit-automatic-open)
		     (= cscope-num-matches-found 1))
		(let ((next-error-found-function #'next-error-quit-window)
		      (current-prefix-arg 0))
		  (next-error))
	      (select-window (get-buffer-window (current-buffer)))
	      (goto-char (point-min))
	      (forward-line 2))
	    (setq cscope-inhibit-automatic-open nil)))))))

(defun cscope-search-message (search)
  "Format a search query for display.
Takes a SEARCH query, which is a cons cell (type . symbol), and
generates a human-readable string describing the search."
  (format "%s: '%s'."
	  (cscope-symbol-title
	   (car (assoc-default (car search) cscope-search-types)))
	  (cdr search)))

(defun cscope-execute-query ()
  "Execute the most recent cscope query.

This function takes the most recent search query from `cscope-searches'
and sends it to the running cscope process.  It first prepares the
cscope buffer by clearing it and displaying the search query."
  (interactive)
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

(defun cscope-point-within-code-context ()
  "Test if the current point is in a code line context."
  (and (not (= (point) (point-max)))
       (>= (line-number-at-pos) 3)
       (let ((current (point)))
	 (save-excursion
	   (goto-char (line-beginning-position))
	   (when (re-search-forward ":[0-9]+:" nil t)
	     (>= current (match-end 0)))))))

(defun cscope-read-string (prompt)
  "Read a string from the minibuffer, adding it to `cscope-history'."
  (let ((initial (cond ((use-region-p)
			(buffer-substring-no-properties (region-beginning)
							(region-end)))
		       ((eq major-mode 'cscope-mode)
			(if (cscope-point-within-code-context)
			    (thing-at-point 'symbol)
			  (cdar cscope-searches)))
		       ((thing-at-point 'symbol)))))
    (read-string prompt initial 'cscope-history)))

(defun cscope-query (&optional type thing)
  "Initiate a cscope search of TYPE for THING."
  (interactive)
  (unless type
    (setf type (completing-read
		"Type: " (mapcar 'cadr cscope-search-types) nil t)))
  (unless thing
    (setf thing
	  (cscope-read-string (concat (cscope-symbol-title type) ": "))))
  (when (stringp type)
    (setf type (car (cl-find type cscope-search-types
			     :key #'cadr :test #'string=))))
  (when (eq (cscope-find-buffer default-directory)
	    (current-buffer))
      (setq cscope-inhibit-automatic-open t))
  (with-current-buffer (cscope-find-buffer default-directory)
    (let ((search (cons type thing)))
      (setq cscope-searches (delete search cscope-searches)
	    cscope-searches (push search cscope-searches)
	    cscope-searches-backup (delete search cscope-searches-backup))
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
      (while (and (< i (abs n))
		  (symbol-value in)
		  (or (eq in 'cscope-searches-backup)
		      (cdr cscope-searches)))
	(set out (push (car (symbol-value in)) (symbol-value out)))
	(set in (cdr (symbol-value in)))
	(cl-incf i)))
    (if (= i (abs n))
	(progn
	  (setq cscope-inhibit-automatic-open t)
	  (cscope-execute-query))
      (message (format "%s of search history."
		       (if (< n 0) "End" "Beginning"))))))

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

(defun cscope-generate-search-functions ()
  "Create interactive search functions from `cscope-search-types'.

For each search type, this function creates a corresponding
interactive function that initiates a cscope query. The function
name is constructed by prefixing 'cscope-' to the search type
name, allowing users to easily call these search functions via
Emacs commands."
  (dolist (feature cscope-search-types)
    (let ((function-name (concat "cscope-" (cadr feature))))
      (fset (intern function-name)
            (lambda ()
              (interactive)
              (cscope-query (cadr feature) nil))))))

(defun cscope-toggle-invisible-property (invisible)
  "Toggle the 'invisible' text property on the current buffer.

This function iterates through all text properties in the current
buffer and toggles the 'invisible' property on those that already
have it.  This is used to show/hide function names in the cscope
results buffer based on the `cscope-show-function` setting."
  (save-excursion
    (let ((pos (point-min))
	  (inhibit-read-only t)
	  next)
      (while (setf next (next-property-change pos))
	(when (memq 'invisible (text-properties-at pos))
	  (put-text-property pos next 'invisible invisible))
	(setf pos next)))))

(defun cscope-re-render-context ()
  "Re-render the code context lines with current settings.

This function re-applies syntax highlighting and match highlighting to
the context lines in the cscope buffer, based on the current values
of `cscope-fontify-code-line` and `cscope-highlight-match`.  It iterates
through each result line and re-renders the context portion."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((inhibit-read-only t))
      (while (re-search-forward ":[0-9]+:\\(.*:\\)?\\([a-zA-z_#\.].*\\)" nil t)
	(let ((context (match-string-no-properties 2)))
	  (delete-region (match-beginning 2) (match-end 2))
	  (cscope-insert-rendered-context context))))))

(defun cscope-generate-toggle-functions ()
  "Create interactive toggle functions from `cscope-display-options'.

The function name is constructed by prefixing 'toggle-' to the
option's variable name, allowing users to toggle these display
options on or off within the cscope interface."
  (dolist (option cscope-display-options)
    (let* ((var (cdr option))
	   (name (symbol-name var)))
      (when (string-prefix-p "cscope-" name)
	(fset (intern (concat "toggle-" name))
	      (lexical-let ((var var)
			    (name name))
		(lambda ()
		  (interactive)
		  (make-local-variable var)
		  (set var (not (symbol-value var)))
		  (cond ((string= name "cscope-show-function")
			 (cscope-toggle-invisible-property
			  (not cscope-show-function)))
			((or (string= name "cscope-highlight-match")
			     (string= name "cscope-fontify-code-line"))
			 (cscope-re-render-context))
			((cscope-execute-query)))
		  (message (format "%s %s" (cscope-symbol-title var)
				   (if (symbol-value var)
				       "enabled."
				     "disabled."))))))))))

(defun cscope-generate-entry-actions ()
    "Create cscope actions for the `cscope-entry' menu.

The actions are built out of the `cscope-search-types'
customizable variable."
  (let ((vec (make-vector (1+ (length cscope-search-types)) "Cscope Actions:")))
    (let ((i 1))
      (dolist (type cscope-search-types)
	(setf (aref vec i)
	      (list (caddr type)
		    (cscope-symbol-title (cadr type))
		    (intern (concat "cscope-" (cadr type)))))
	(cl-incf i)))
    vec))

(defun cscope-generate-toggle-actions ()
  "Create display toggle actions for the `cscope-entry' menu.

The actions are built out of the `cscope-display-options'
customizable variable."
  (let ((vec (make-vector (1+ (length cscope-display-options)) "Toggle:"))
        (i 1))
    (dolist (option cscope-display-options)
      (let ((var (cdr option))
            (key (car option)))
        (setf (aref vec i)
              (list key
                    (cscope-symbol-title var)
                    (intern (concat "toggle-" (symbol-name var))))))
      (cl-incf i))
    vec))

(defun cscope--init ()
  "Initializes cscope mode by generating search functions, toggle
functions, and transient menu actions."
  (cscope-generate-search-functions)
  (cscope-generate-toggle-functions)
  (transient-replace-suffix 'cscope-entry '(0)
    (cscope-generate-entry-actions))
  (transient-replace-suffix 'cscope-toggle '(0)
    (cscope-generate-toggle-actions)))

(defun cscope-generate-toggle-mode-line ()
  "Create the mode line entries for cscope display options."
  (mapcar (lambda (option)
	    `(:eval
	      (propertize ,(car option)
			  'face (if ,(cdr option)
 				    'compilation-mode-line-exit
				  'compilation-error)
			  'help-echo ,(cscope-symbol-title (cdr option)))))
	  cscope-display-options))

(defun cscope-switch-to-buffer ()
  "Switch to a cscope buffer.

This function lists all buffers currently in `cscope-mode` and
prompts the user to select one. It uses `ido-mode` for completion
if available, providing an interactive buffer-switching experience.

When called, it switches to the selected buffer, allowing the
user to quickly navigate between different cscope result buffers."
  (interactive)
  (let* ((buffers (mapcar 'buffer-name (cscope-buffers)))
	 (prompt "Buffer: ")
	 (buffer (if (and (boundp 'ido-mode) ido-mode)
		     (ido-completing-read prompt buffers)
		   (completing-read prompt buffers))))
    (switch-to-buffer buffer)))

(transient-define-prefix cscope-entry ()
  "Defines a transient menu cscope."
  ["Place holder"]
  ["Database"
   ("G" "Regenerate" cscope-generate-database)]
  ["Buffer(s)"
   ("b" "Switch to a buffer" cscope-switch-to-buffer)]
  (interactive)
  (transient-setup 'cscope-entry))

(transient-define-prefix cscope-toggle ()
  "Defines a transient menu to toggle cscope buffer display options."
  ["Place holder"]
  (interactive)
  (transient-setup 'cscope-toggle))

(defun cscope-goto-match ()
  "Navigate to the location of the current match.

This function uses `compile-goto-error' to jump to the source
location corresponding to the current error highlighted in the
cscope results buffer. It temporarily sets
`next-error-found-function' to `next-error-quit-window' and
`current-prefix-arg' to 0 to ensure the cscope buffer is closed
after jumping to the error."
  (interactive)
  (let ((next-error-found-function #'next-error-quit-window)
	(current-prefix-arg 0))
    (compile-goto-error)))

(defun cscope-display-error ()
  "Display error at the current point in another window.

This function checks if the current buffer is in `cscope-mode`.
If it is, it sets `next-error-last-buffer` to the current buffer.
This ensures that when `compilation-display-error` is called, it knows
which buffer to refer to for displaying the error."
  (interactive)
  (when (eq major-mode 'cscope-mode)
    (setq next-error-last-buffer (current-buffer))
    (compilation-display-error)))

(defun cscope-current-error-buffer ()
  (save-excursion
    (let* ((err (compilation-next-error 0 nil compilation-current-error))
	   (loc (compilation--message->loc err))
	   (file (caar (compilation--loc->file-struct loc)))
	   (path (concat (expand-file-name
			  (concat default-directory "/" file)))))
      (cl-find path (buffer-list) :test 'string= :key 'buffer-file-name))))

(defun cscope-quit-current-error ()
  "Close the window displaying the current error if visible."
  (interactive)
  (save-selected-window
    (when-let ((buffer (cscope-current-error-buffer)))
      (when-let ((window (get-buffer-window buffer)))
	(with-selected-window window
          (quit-window))))))

(defun cscope-kill-current-error-buffer ()
  (interactive)
  "Kill the buffer of the current error."
  (when-let ((buffer (cscope-current-error-buffer)))
    (kill-buffer buffer)))

(defvar cscope-mode-map (cl-copy-list grep-mode-map))
(define-key cscope-mode-map (kbd "e") #'cscope-entry)
(define-key cscope-mode-map (kbd "t") #'cscope-toggle)
(define-key cscope-mode-map (kbd "g") #'cscope-execute-query)
(define-key cscope-mode-map (kbd "G") #'cscope-generate-database)
(define-key cscope-mode-map (kbd "P") #'cscope-previous-query)
(define-key cscope-mode-map (kbd "N") #'cscope-next-query)
(define-key cscope-mode-map (kbd "C-q") #'cscope-quit-current-error)
(define-key cscope-mode-map (kbd "C-k") #'cscope-kill-current-error-buffer)
(define-key cscope-mode-map (kbd "<return>") #'cscope-goto-match)

;;;###autoload
(define-compilation-mode cscope-mode "Cscope"
  "Major mode for displaying cscope search results.

This mode is derived from `compilation-mode', providing features
like `next-error' and `previous-error' to navigate through search
results. The mode line displays the number of matches found."
  (let ((mode-line `(" [" (:propertize
			   (:eval (int-to-string cscope-num-matches-found))
			   face ,grep-hit-face
			   help-echo "Number of matches so far")
		     " " ,@(cscope-generate-toggle-mode-line) "]")))
    (setq-local compilation-error-regexp-alist grep-regexp-alist
		compilation-error-face grep-hit-face
		compilation-mode-line-errors mode-line)
    (cscope--init)))

;; Make sure function and menu are initialized on load
(cscope--init)
(provide 'cscope)
