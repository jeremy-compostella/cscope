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
(require 'tree-widget)
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

(defcustom cscope-file-patterns '("*.[chsS]")
  "List of file patterns to search for cscope database generation.")

(defcustom cscope-search-types
  '((0 "find-this-C-symbol" "," font-lock-function-name-face)
    (1 "find-this-function-definition" "." font-lock-function-name-face)
    (2 "find-functions-called-by-this-function" "c" font-lock-function-name-face t)
    (3 "find-functions-calling-this-function" "C" font-lock-function-name-face t)
    (4 "find-this-text-string" "s" font-lock-string-face)
    (5 "change-this-text-string" "S" font-lock-string-face)
    (6 "find-this-egrep-pattern" "e" font-lock-string-face)
    (7 "find-this-file" "f" compilation-info)
    (8 "find-files-including-file" "i" compilation-info)
    (9 "find-assignments-to-this-symbol" "a" font-lock-function-name-face))
  "Lists mapping cscope search type numbers to their descriptive
names, a font to use to represent the search object, and an
optional flag for tree mode view."
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

(defcustom cscope-persistent-filters nil
  "Whether filters persist between queries in the same buffer.

When non-nil, filters applied to a cscope results buffer will
remain active even when running new queries in that buffer.

When nil, filters are cleared each time a new query is executed,
starting with a fresh, unfiltered result set."
  :type 'boolean)

(defcustom cscope-tree-max-depth 3
  "Maximum depth of the cscope tree to preload."
  :type 'integer)

(defcustom cscope-highlight-face 'highlight
  "Face used to highlight matching symbols in cscope results."
  :type 'face)

(defvar cscope-lock nil
  "A lock variable used to prevent concurrent cscope searches.

If non-nil, indicates that a cscope search is currently in
progress, thus preventing initiation of additional searches until
the current one completes.")

(defvar cscope-history '()
  "History list for symbols queried by cscope search functions.")

(defvar cscope-filter-history '()
  "History list for filter regular expressions.")

(defvar-local cscope-filters '()
  "List of filters applied to the cscope results buffer.")

(defvar-local cscope-process nil
  "Process object for the running cscope command.")

(defvar-local cscope-searches nil
  "A list of previous searches performed in this buffer.

Each element is a (type . thing) cons cell where type is the
index of the type of cscope search and thing is the string being
looked for.")

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

(defvar-local cscope-message-unique-match nil
  "Determine if a message should be displayed for a unique match.

When non-nil, it indicates if a unique match is found during a
cscope search, and a message should be displayed instead of
jumping directly to the match.

This is typically used to notify to reduce the disturbance of
windows change.")

(defvar-local cscope-tree-requests '()
  "A list of pending requests for building the cscope call tree.

Each element in this list represents a request to fetch and
insert the children of a particular node in the cscope call tree.
The format of each request is a list containing:

- TYPE: The cscope search type (e.g., find functions called by).
- THING: The symbol or string to search for (the function name).
- PARENT: The tree widget representing the parent node (or nil
  for the root).
- DEPTH: The current depth of the tree being built.
- MARKER: A buffer position marker.

This variable is used in conjunction with `cscope-tree-insert-match'
and `cscope-tree-search-complete' to manage the asynchronous
population of the cscope call tree.")

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

(defun cscope-search-type ()
  "Return the type, as a number, of the current cscope search."
  (caar cscope-searches))

(defun cscope-search-thing ()
  "Return the 'thing' part of the current cscope search.

The 'thing' is the symbol or string that was searched for."
  (cdar cscope-searches))

(defun cscope-search-thing-property ()
  "Font-lock face associated with the current search type."
  (caddr (assoc-default (cscope-search-type) cscope-search-types)))

(defun cscope-search-propertized-thing ()
  "Current search 'thing' propertized with the appropriate face."
  (let ((face (cscope-search-thing-property)))
    (propertize (cscope-search-thing) 'font-lock-face face)))

(defun cscope-search-tree-p ()
  "Whether the current cscope search should be displayed as a tree."
  (cadddr (assoc-default (cscope-search-type) cscope-search-types)))

(defun cscope-set-process-filter ()
  "Set the cscope process filter based on the search type.

If the current search is a tree-based search (e.g., call graph),
the filter is set to use `cscope-tree-insert-match' and
`cscope-tree-search-complete'.  Otherwise, it uses
`cscope-insert-match' and `cscope-search-complete' for regular
flat result display."
  (let ((insert (if (cscope-search-tree-p)
		    #'cscope-tree-insert-match
		  #'cscope-insert-match))
	(complete (if (cscope-search-tree-p)
		      #'cscope-tree-search-complete
		    #'cscope-search-complete)))
    (set-process-filter cscope-process
			(apply-partially #'cscope-filter insert complete))))

(defun cscope-start-process ()
  "Start the cscope process in line-oriented mode.
Uses the database file 'cscope.out' in the current default directory.
Sets the `cscope-process' variable and assigns `cscope-filter' as
the process filter function."
  (cl-flet ((insert-match (file function line context) t)
	    (complete ()
	      (cscope-set-process-filter)
	      (setq cscope-lock nil)))
    (let ((filter (apply-partially #'cscope-filter #'insert-match #'complete)))
      (setq cscope-lock t
	    cscope-leftover nil)
      (setq cscope-process
	    (start-file-process "cscope" (current-buffer) "cscope" "-ld" "-f"
				"cscope.out"))
      (set-process-filter cscope-process filter)
      (while cscope-lock
	(sleep-for .1)))))

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

(defun cscope-generate-database-command (&optional ignore-make)
  "Generate the command string to create the cscope database.

If any root directory Makefile contains a reference to cscope, it
assumes there is a cscope make target."
  (if (and (not ignore-make)
	   (= (shell-command "grep cscope Makefile*") 0))
      "make cscope"
    (let ((find-params (mapconcat (lambda (p)
				    (format " -name '%s' " p))
				  cscope-file-patterns "-o"))
	  (cscope-params "-b -q"))
      (format "find . \\( %s \\) > cscope.files && cscope %s"
	      find-params cscope-params))))

(defmacro with-cscope-buffer (&rest body)
  "Execute BODY within cscope buffer linked to current directory."
  (declare (indent 0))
  `(with-current-buffer (cscope-find-buffer default-directory)
     (progn ,@body)))

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
  (with-cscope-buffer
    (when-let ((process (get-buffer-process (current-buffer))))
      (when (process-live-p process)
	(kill-process process)))
    (let* ((command (cscope-generate-database-command))
	   (process (start-file-process-shell-command
		     "cscope" (current-buffer) command))
	   (progress (make-progress-reporter
		      (format "Generating cscsope database for %s..."
			      default-directory)))
	   (timer-func (lexical-let ((progress progress))
			 (apply-partially #'progress-reporter-update progress)))
	   (timer (run-at-time .5 2 timer-func)))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (format "Generating database using the '%s' command." command)))
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

(defun cscope-move-prop (from to)
  "Move text properties from FROM to TO within a buffer."
  (let ((pos (point-min))
	next)
    (while (and (not (= pos (point-max)))
		(setf next (or (next-property-change pos) (point-max))))
      (when-let ((prop (get-text-property pos from)))
	(put-text-property pos next to prop)
	(put-text-property pos next from nil))
      (setf pos next))))

(defun cscope-get-file-mode (file)
  "Determine the major mode for a file based on `auto-mode-alist'.

Returns the mode specifier (a function symbol) or nil if no match
is found."
  (cl-flet ((match (x)
	      (when (string-match (car x) file)
		(cons (match-end 0) (cdr x)))))
    (let ((matches (delq nil (mapcar #'match auto-mode-alist))))
      (when matches
	(let ((best (apply #'max (mapcar 'car matches))))
	  (assoc-default best matches))))))

(defun cscope-fontify (file context)
  "Apply syntax highlighting to CONTEXT string.

This function attempts to apply syntax highlighting to the given
CONTEXT string based on the FILE type."
  (if-let ((mode (cscope-get-file-mode file)))
      (with-temp-buffer
	(insert context)
	(cl-letf (((symbol-function 'run-mode-hooks) #'ignore))
          (funcall mode))
	(font-lock-ensure (point-min) (point-max))
	(cscope-move-prop 'face 'font-lock-face)
	(buffer-string))
    context))

(defun cscope-highlight-match-in-line (&optional thing)
  (when cscope-highlight-match
    (let ((thing (or thing (cscope-search-thing))))
      (when (search-forward thing (line-end-position) t)
	(let ((ol (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put ol 'face cscope-highlight-face))))))

(defun cscope-insert-rendered-context (file context)
  "Renders CONTEXT with optional fontification and highlighting.

This function takes a CONTEXT string, which represents a line of
code found by cscope, and applies optional transformations based
on customizable variables."
  (setf context (string-trim-left context))
  (when cscope-fontify-code-line
    (setf context (cscope-fontify file context)))
  (save-excursion
    (insert context))
  (cscope-highlight-match-in-line)
  (goto-char (line-end-position)))

(defun cscope-hide-match (regexp)
  "Hide the current match in the cscope buffer.

This function hides the current line by setting the `invisible'
text property.  The REGEXP is stored in the 'invisible' text
property, allowing the line to be unhidden later."
  (let ((begin (1- (line-beginning-position)))
        (end (line-end-position))
        (inhibit-read-only t))
    (put-text-property begin end 'invisible regexp)
    (remove-overlays begin end 'face cscope-highlight-match)))

(defun cscope-show-match ()
  "Show the current match in the cscope buffer."
  (let ((pos (1- (line-beginning-position)))
	(end (line-end-position))
	(inhibit-read-only t)
	next)
    (remove-text-properties pos end '(invisible nil))
    ;; Set-up invisible property according to `cscope-show-function'
    (while (and (setf next (next-property-change pos)) (< next end))
      (let ((props (text-properties-at pos)))
	(when (memq 'cscope-function props)
	  (put-text-property pos next 'invisible (not cscope-show-function))))
      (setf pos next))
    ;; Render the context according to `cscope-highlight-match' and
    ;; `cscope-fontify-code-line'
    (let* ((loc (cscope-match-loc))
	   (file (caar (compilation--loc->file-struct loc)))
	   (line (compilation--loc->line loc))
	   (fun-rexp "[a-zA-Z0-9-_]+:"))
      (while (re-search-forward (format "%s:%d:\\(%s\\)?\\(.*\\)"
					file line fun-rexp)
				(line-end-position) t)
	(let ((context (match-string-no-properties 2)))
	  (delete-region (match-beginning 2) (match-end 2))
	  (cscope-insert-rendered-context file context))))))

(defun cscope-invisible-match ()
  "Check if the current line is marked as invisible by a filter.

Returns the regular expression used to hide the line, if the line has
been marked as invisible by `cscope-hide-match'. Returns nil otherwise."
  (get-text-property (line-beginning-position) 'invisible))

(defun cscope-should-hide-match (regexp)
  "Return whether the current line should be hidden based on REGEXP.

If the 'cscope-filter-out' text property is set for REGEXP, this
function returns t if REGEXP is found in the current
line (meaning the line should be hidden because it matches the
exclusion pattern).

If 'cscope-filter-out' is not set, it returns t if REGEXP is not
found in the current line (meaning the line should be hidden
because it doesn't match the inclusion pattern)."
  (save-excursion
    (if (get-text-property 0 'cscope-filter-out regexp)
	(re-search-forward regexp (line-end-position) t)
      (not (re-search-forward regexp (line-end-position) t)))))

(defun cscope-filter-match ()
  (if-let* ((reasons (delq nil (mapcar #'cscope-should-hide-match
				       cscope-filters)))
	    (regexp (car reasons)))
      (progn
	(cscope-hide-match regexp)
	nil)
    (cscope-show-match)
    t))

(defun cscope-print-help ()
  "Display helpful keybindings in the minibuffer."
  (interactive)
  (let ((keys '((cscope-next-match . "next match")
		(cscope-previous-match . "previous match")
		(next-error-no-select . "opening next match")
		(previous-error-no-select . "opening previous match")
		(compilation-next-file . "next file")))
	(last-key '(compilation-previous-file . "previous file")))
    (when (cscope-search-tree-p)
      (setf keys (append '((cscope-tree-toggle . "(un)fold tree node")
			   (cscope-tree-up . "parent"))
			 keys)))
    (when (cdr cscope-searches)
      (setf keys (append keys (list last-key)))
      (setf last-key '(cscope-previous-query . "previous cscope query")))
    (when cscope-searches-backup
      (setf keys (append keys (list last-key)))
      (setf last-key '(cscope-next-query . "next cscope query")))
    (cl-flet ((format-key (key)
		(format "\\[%s] for %s" (symbol-name (car key)) (cdr key))))
      (message "Hit %s and %s."
	       (substitute-command-keys (mapconcat #'format-key keys ", "))
	       (substitute-command-keys (format-key last-key))))))

(defun cscope-display-buffer ()
  (let* ((buffer (current-buffer))
	 (window (get-buffer-window buffer)))
    (unless (equal (frame-selected-window) window)
      (if window
	  (select-window window)
	(select-window (display-buffer buffer)))
      (goto-char (cscope-first-error-position))
      (cscope-print-help))))

(defun cscope-insert-match (file function line context)
  "Insert a cscope search result into BUFFER.

The result is formatted as 'file:line: context\\n'.
Increments `cscope-num-matches-found'.
If this is the second match found, displays the buffer.
Highlights the search symbol in the context."
  (cl-incf cscope-num-matches-found)
  (when (= cscope-num-matches-found 1)
    (setq next-error-last-buffer (current-buffer)))
  (when (and cscope-fontify-code-line
	     (= cscope-num-matches-found
		cscope-highlight-and-font-line-limit))
    (message "Fontification limit reached, disabling fontification."))
  (let* ((inhibit-read-only t)
	 (below-limit (< cscope-num-matches-found
			 cscope-highlight-and-font-line-limit))
	 (cscope-fontify-code-line (and below-limit
					cscope-fontify-code-line))
	 (cscope-highlight-match-in-line (and below-limit
					      cscope-highlight-match)))
    (save-excursion
      (goto-char (point-max))
      (let ((fun (if (or (string= function "<global>")
			 (string= function "<unknown>")
			 (string= function (cscope-search-thing)))
		     ""
		   (propertize
		    (concat (propertize function 'face nil
					'font-lock-face
					'font-lock-function-name-face)
			    ":")
		    'cscope-function t
		    'invisible (not cscope-show-function)))))
        (insert (format "%s:%s:%s%s\n" file line fun context))
	(save-excursion
	  (forward-line -1)
	  (unless (cscope-filter-match)
	    (cl-decf cscope-num-matches-found))))))
  (when (= cscope-num-matches-found 2)
    (cscope-display-buffer)))

(defun cscope-search-mark-complete ()
  (let ((face (if (= cscope-num-matches-found 0)
		  'compilation-mode-line-fail
		'compilation-mode-line-exit)))
    (setq mode-line-process
	  `((:propertize ":exit" face ,face) compilation-mode-line-errors)
	  cscope-inhibit-automatic-open nil
	  cscope-lock nil)))

(defun cscope-search-complete ()
  "Actions to perform after a cscope search completes.

This function is called after the cscope process finishes and all
output has been processed. It unlocks the cscope buffer, updates
the mode line to reflect the search status (success or failure),
displays a message if no matches were found, and navigates to the
first match if appropriate."
  (if (= cscope-num-matches-found 0)
      (message "No match found for '%s'." (cscope-search-thing))
    (if (and (not cscope-inhibit-automatic-open)
	     (= cscope-num-matches-found 1)
	     (not (cscope-search-tree-p)))
	(if cscope-message-unique-match
	    (cscope-message-unique-match)
	  (let ((next-error-found-function #'next-error-quit-window)
		(current-prefix-arg 0))
	    (next-error)))
      (cscope-display-buffer)))
  (cscope-search-mark-complete))

(defmacro for-all-cscope-match (&rest body)
  "Execute BODY for each cscope match in the current buffer."
  (declare (indent 0))
  `(with-cscope-buffer
     (save-excursion
       (goto-char (cscope-first-error-position))
       (forward-line 2)
       (while (not (eobp))
	 (progn ,@body)
	 (forward-line)))))

(defun cscope-is-busy ()
  "Check if the current buffer cscope process is running and locked."
  (when-let ((process (get-buffer-process (current-buffer))))
    (and (eq (process-status process) 'run) cscope-lock)))

(defun cscope-message-unique-match ()
  "Display a message showing the unique match found.

This function extracts the content of the first visible match in the
cscope buffer, applies any syntax highlighting present, and displays
the result as a message in the minibuffer.  It then resets the
`cscope-message-unique-match' flag to nil."
  (goto-char (cscope-first-error-position))
  (let ((match (buffer-substring (line-beginning-position) (line-end-position))))
    (message
     (with-temp-buffer
       (insert match)
       (cscope-move-prop 'font-lock-face 'face)
       (buffer-string))))
  (setq cscope-message-unique-match nil))

(defun cscope-filter (insert-fun complete-fun process output)
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
      (let* ((rpath "\\([a-zA-Z0-9_<>/\.\-]+\\)")
	     (rfunction "\\([a-zA-Z0-9_<>]+\\)")
	     (rnumber "\\([0-9]+\\)")
	     (regexp (format "^%s %s %s \\(.*\\)$" rpath rfunction rnumber)))
	(while (re-search-forward regexp nil t)
	  (if (= (point-max) (line-end-position))
	      (cscope-backup-incomplete-line buffer)
	    (let ((file (match-string 1))
		  (function (match-string  2))
		  (line (string-to-number (match-string 3)))
		  (context (match-string 4)))
	      (with-current-buffer buffer
		(funcall insert-fun file function line context)))))
	(unless (eobp)
	  (forward-char))
	(when (and (= (point) (line-beginning-position))
		   (not (= (point) (line-end-position))))
	  (cscope-backup-incomplete-line buffer)))
      ;; End of data
      (goto-char (point-max))
      (forward-line -1)
      (when (re-search-forward "^>>" nil t)
	(with-current-buffer buffer
	  (funcall complete-fun))))))

(defun cscope-print-filters ()
  "Display the active filters in the cscope buffer's header line."
  (cl-flet ((filter-string (regexp)
	      (concat (if (get-text-property 0 'cscope-filter-out regexp)
			  "!" "")
		      (propertize regexp 'font-lock-face 'match))))
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (let ((inhibit-read-only t))
	(delete-region (line-beginning-position) (line-end-position))
	(when cscope-filters
	  (insert (format "Filters: %s"
			  (mapconcat #'filter-string
				     cscope-filters ", "))))))))

(defun cscope-search (type thing)
  (cscope-set-process-filter)
  (process-send-string cscope-process (format "%d%s\n" type thing)))

(defun cscope-tree-tag (function &optional file line context)
  (cl-flet ((propertize-function (function)
	      (propertize function 'font-lock-face
			  (cscope-search-thing-property))))
    (let* ((location (when (and file line)
		       (format "%s:%s:" file line)))
	   (function (unless (member function '("<global>" "<unknown>"))
		       (propertize function 'font-lock-face
				   (cscope-search-thing-property))))
	   (prefix (if (and file context (= (cscope-search-type) 2))
		       (cscope-fontify file context)
		     function))
	   (tag (mapconcat #'identity (delq nil (list prefix location))  " ")))
      (propertize tag 'cscope-thing (if (= (cscope-search-type) 8)
					(file-name-nondirectory file)
				      function)))))

(defun cscope-tree-widget (type function file line context insert)
  "Create a tree widget for a cscope match."
  (if insert
      (save-excursion
	(goto-char (point-max))
	(let (node)
	  (save-excursion
	    (setf node (widget-create 'tree-widget
				      :tag (cscope-tree-tag function file line context)
				      :help-echo nil)))
	  (cscope-highlight-match-in-line (cscope-tree-thing node))
	  node))
    (widget-convert 'tree-widget
                    :tag (cscope-tree-tag function file line context)
		    :help-echo nil)))

(defun cscope-tree-should-hide-match (file function line context)
  "Check if the current match should be hidden based on filters."
  (let ((filters cscope-filters))
    (with-temp-buffer
      (save-excursion
	(insert (format "%s:%s:%s %s\n" file line function context)))
      (delq nil (mapcar #'cscope-should-hide-match filters)))))

(defun cscope-tree-thing (node)
  (get-text-property 0 'cscope-thing (widget-get node :tag)))

(defun cscope-tree-insert-match (file function line context)
  (when cscope-tree-requests
    (unless (cscope-tree-should-hide-match file function line context)
      (cl-multiple-value-bind (type parent widget depth) (car cscope-tree-requests)
	(let* ((child (cscope-tree-widget type function file line context
					  (not widget)))
	       (childs (when widget
			 (widget-get widget :args))))
	  (cl-incf cscope-num-matches-found)
	  (when widget
	    (widget-put widget :args (append childs (list child))))
	  (when (< depth cscope-tree-max-depth)
	    (let ((request (list type (cscope-tree-thing child)
				 child (1+ depth))))
	      (setq cscope-tree-requests (append cscope-tree-requests
						 (list request))))))))))

(defun cscope-tree-build-childs (node)
  (interactive)
  (let* ((thing (cscope-tree-thing node))
	 (request (list (list (cscope-search-type) thing node
			      cscope-tree-max-depth (point-marker)))))
    (if (not cscope-tree-requests)
	(setq cscope-tree-requests request)
      (setcdr request (cdr cscope-tree-requests))
      (setcdr cscope-tree-requests request))
    (unless (cdr cscope-tree-requests)
      (cscope-tree-execute-request))))

(defun cscope-tree-execute-request ()
  (let ((request (car cscope-tree-requests)))
    (unless (process-live-p cscope-process)
      (cscope-start-process))
    (setq mode-line-process
	  '((:propertize ":run" face compilation-mode-line-run)
            compilation-mode-line-errors))
    (process-send-string cscope-process
			 (format "%d%s\n" (nth 0 request) (nth 1 request)))))

(defun cscope-tree-widget-after-toggle (node)
  (save-excursion
    (dolist (child (widget-get node :children))
      (goto-char (widget-get child :from))
      (let ((inhibit-read-only t))
	(compilation--parse-region (line-beginning-position)
				   (1+ (line-end-position))))
      (cscope-highlight-match-in-line (cscope-tree-thing child))
      (cscope-tree-widget-after-toggle child))))

(defun cscope-tree-search-complete ()
  (let ((request (car cscope-tree-requests)))
    (when-let ((marker (nth 4 request)))
      (save-excursion
	(goto-char marker)
	(when-let ((node (widget-get (get-char-property (point) 'button) :parent)))
	  (when (widget-get node :args)
	    (widget-value-set node (widget-value node))
	    (widget-button-press (point))))))
    (setq cscope-tree-requests (cdr cscope-tree-requests))
    (if cscope-tree-requests
	(cscope-tree-execute-request)
      (cscope-search-mark-complete)
      (if (= cscope-num-matches-found 0)
	  (message "No match found for '%s'." (cscope-search-thing))
	(cscope-display-buffer)))))

(defun cscope-tree-search (process type thing)
  (let* ((tag (cscope-search-propertized-thing)))
    (cscope-set-process-filter)
    (setq cscope-tree-requests (list (list type thing nil 1)))
    (cscope-tree-execute-request)))

(defun cscope-prepare-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s: '%s'.\n\n"
		    (cscope-symbol-title
		     (car (assoc-default (cscope-search-type)
					 cscope-search-types)))
		    (cscope-search-propertized-thing)))
    (cscope-print-filters)))

(defun cscope-execute-query ()
  "Execute the most recent cscope query from `cscope-searches'.

This function retrieves the latest search query stored in
`cscope-searches' and sends it to the active cscope process.
Prior to executing the query, it ensures the cscope buffer is
prepared by clearing its contents and displaying the search
details. If the cscope process is not running or the database
file 'cscope.out' is absent, it attempts to start the process or
generate the database, respectively."
  (interactive)
  (when (cscope-is-busy)
    (cscope-kill-compilation))
  (cscope-prepare-buffer)
  (setq mode-line-process
	'((:propertize ":run" face compilation-mode-line-run)
          compilation-mode-line-errors)
	cscope-num-matches-found 0)
  (unless (and cscope-process (process-live-p cscope-process))
    (if (file-readable-p "cscope.out")
	(cscope-start-process)
      (cscope-generate-database)))
  (when (and cscope-process (process-live-p cscope-process))
    (setq cscope-lock t)
    (let ((type (cscope-search-type))
	  (thing (cscope-search-thing)))
      (if (cscope-search-tree-p)
	  (cscope-tree-search cscope-process type thing)
	(cscope-search type thing)))))

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
			(cond ((cscope-search-tree-p)
			       (thing-at-point 'symbol))
			      ((cscope-point-within-code-context)
			       (thing-at-point 'symbol))
			      ((cscope-search-thing))))
		       ((thing-at-point 'symbol)))))
    (read-string prompt initial 'cscope-history)))

(defun cscope-query (&optional type thing)
  "Initiate a cscope search of TYPE for THING.

This function initiates a cscope search for a given type and symbol.
It handles reading the symbol from the minibuffer, determining the
cscope buffer, applying filters (including those specified via the
transient menu), and executing the cscope query.

TYPE: The type of cscope search (e.g., find-this-C-symbol). If not
provided, the user is prompted to select a type from a list.

THING: The symbol or string to search for. If not provided, the user
is prompted to enter a symbol or string in the minibuffer.

Filters are applied based on the following:

- The 'cscope-persistent-filters' variable determines whether filters
  persist between queries. If nil, filters are cleared before each
  new query.

- The transient menu ('cscope-entry') allows specifying include and
  exclude filters using regular expressions.

- The 'limit-to-subdir' option in the transient menu limits the
  search to the current subdirectory."
  (interactive)
  (cl-flet ((push-filter (buffer regexp)
	      (with-current-buffer buffer
		(add-to-list 'cscope-filters regexp t))))
    (setf type (cond ((not type)
		      (completing-read
		       "Type: " (mapcar 'cadr cscope-search-types) nil t))
		     ((numberp type)
		      (car (assoc-default type cscope-search-types)))
		     (type)))
    (unless thing
      (setf thing
	    (cscope-read-string (concat (cscope-symbol-title type) ": "))))
    (when (stringp type)
      (setf type (car (cl-find type cscope-search-types
			       :key #'cadr :test #'string=))))
    (let ((cscope-buffer (cscope-find-buffer default-directory))
	  (args (transient-args 'cscope-entry)))
      (unless cscope-persistent-filters
	(with-current-buffer cscope-buffer
	  (setq cscope-filters '())))
      (when (eq cscope-buffer (current-buffer))
	(setq cscope-inhibit-automatic-open t))
      (when-let ((filter-in (transient-arg-value "filter-in=" args)))
	(push-filter cscope-buffer filter-in))
      (when-let ((filter-out (transient-arg-value "filter-out=" args)))
	(push-filter cscope-buffer (propertize filter-out
					       'cscope-filter-out t)))
      (when current-prefix-arg
	(with-current-buffer cscope-buffer
	  (setq cscope-message-unique-match t)))
      (when-let ((directory (or (transient-arg-value "limit-to-subdir=" args)
				(when (member "limit-to-subdir" args)
				  (default-directory)))))
	(setf directory (expand-file-name directory))
	(let* ((cscope-directory (with-current-buffer cscope-buffer
				   default-directory))
	       (length (length cscope-directory)))
	  (when (string-prefix-p cscope-directory directory)
	    (push-filter cscope-buffer
			 (concat "^" (substring directory (1+ length)))))))
      (with-current-buffer cscope-buffer
	(let ((search (cons type thing)))
	  (setq cscope-searches (delete search cscope-searches)
		cscope-searches (push search cscope-searches)
		cscope-searches-backup (delete search cscope-searches-backup))
	  (cscope-execute-query))))))

(defun cscope-re-execute-query ()
  "Re-execute the most recent cscope query.

This function provides a mechanism to re-run the last cscope
query. If called with a universal prefix argument (C-u), it will
re-initiate the query using the same search type as the most
recent one in `cscope-searches` but asks for a pattern to search
for."
  (interactive)
    (if (equal current-prefix-arg '(4))
	(cscope-query (caar cscope-searches))
      (cscope-execute-query)))

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
	  (unless cscope-persistent-filters
	    (setq cscope-filters '()))
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
              (cscope-query (cadr feature)))))))

(defun cscope-match-loc ()
  "Return the compilation--loc structure of the current match."
  (let ((err (compilation-next-error 0)))
    (compilation--message->loc err)))

(defun cscope-refresh (&optional visible)
  "Refreshes the cscope buffer, reapplying filters and highlighting.

This function iterates through each match in the cscope buffer,
re-evaluating filters and reapplying syntax highlighting and
match highlighting based on current settings.  It also manages
the `cscope-num-matches-found' counter and displays a progress
reporter for large buffers.

VISIBLE: If non-nil, only refresh visible matches (those not
already filtered out).  This can improve performance when only
updating the display."
  (let* ((line-count (line-number-at-pos (point-max)))
	 progress)
    (setq cscope-num-matches-found 0)
    (for-all-cscope-match
      (unless (and visible (cscope-invisible-match))
        (when (and cscope-fontify-code-line
                   (= cscope-num-matches-found
		      cscope-highlight-and-font-line-limit))
          (message "Fontification limit reached, disabling fontification."))
        (let* ((below-limit (< cscope-num-matches-found
			       cscope-highlight-and-font-line-limit))
	       (cscope-fontify-code-line (and below-limit
					      cscope-fontify-code-line))
	       (cscope-highlight-match-in-line (and below-limit
						    cscope-highlight-match)))
          (unless below-limit
            (unless progress
	      (setf progress (make-progress-reporter "Refreshing cscope buffer"
                                                     0 line-count)))
            (progress-reporter-update progress (line-number-at-pos)))
          (when (cscope-filter-match)
            (cl-incf cscope-num-matches-found)))))
      (when progress
	(progress-reporter-done progress))))

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
		  (cscope-refresh t)
		  (message (format "%s %s" (cscope-symbol-title var)
				   (if (symbol-value var)
				       "enabled."
				     "disabled."))))))))))

(defun cscope-generate-entry-actions ()
  "Create cscope actions for the `cscope-entry' menu.

The actions are built out of the `cscope-search-types'
customizable variable."
  (let ((vec (make-vector (1+ (length cscope-search-types)) "Cscope Actions:"))
	(i 1))
    (dolist (type cscope-search-types)
      (setf (aref vec i)
	    (list (caddr type)
		  (cscope-symbol-title (cadr type))
		  (intern (concat "cscope-" (cadr type)))))
      (cl-incf i))
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
  (transient-replace-suffix 'cscope-entry '(1)
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
    (if-let ((window (get-buffer-window buffer)))
	(select-window window)
      (switch-to-buffer buffer))))

(defun cscope-quit-buffer ()
  "Quit the current cscope buffer.

This provides a convenient way to dismiss cscope result windows."
  (interactive)
  (when-let ((buffer (cscope-find-buffer default-directory)))
    (when-let ((window (get-buffer-window buffer)))
      (with-selected-window window
	(quit-window)))))

(transient-define-argument cscope-transient-read-directory ()
  :description "Limit matches to a specific directory"
  :class 'transient-option
  :argument "limit-to-subdir="
  :allow-empty t
  :reader #'transient-read-directory)

(transient-define-argument cscope-transient-read-filter ()
  "Define a transient argument for filtering cscope results by regexp."
  :description "Filter results by regular expression"
  :class 'transient-option
  :argument "filter-in="
  :allow-empty t
  :reader #'read-string)

(transient-define-argument cscope-transient-read-filter-out ()
  "Define a transient argument for filtering cscope results by regexp."
  :description "Filter OUT results by regular expression"
  :class 'transient-option
  :argument "filter-out="
  :allow-empty t
  :reader #'read-string)

(transient-define-prefix cscope-entry ()
  "Defines a transient menu cscope."
  ["Filters Options:"
   ("-s" "Limit results to current sub-directory" ("-s" "limit-to-subdir"))
   ("-d" cscope-transient-read-directory)
   ("-i" cscope-transient-read-filter)
   ("-o" cscope-transient-read-filter-out)]
  ["Place holder"]
  ["Database"
   ("G" "Regenerate" cscope-generate-database)]
  ["Buffer(s)"
   ("b" "Switch to a cscope buffer" cscope-switch-to-buffer)
   ("q" "Quit cscope buffer" cscope-quit-buffer)]
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

(defmacro cscope-on-match (&rest body)
  "Execute BODY only when the current buffer is in `cscope-mode'.

This macro checks if the current buffer's major mode is
`cscope-mode'. If it is, it sets `next-error-last-buffer' to the
current buffer and then executes the code provided in BODY. It
ensures that navigation commands like `next-error' operate
correctly within that context."
  (declare (indent 0))
  `(when (eq major-mode 'cscope-mode)
     (setq next-error-last-buffer (current-buffer))
     (progn ,@body)))

(defun cscope-display-match ()
  "Display error at the current point in another window.

This function checks if the current buffer is in `cscope-mode`.
If it is, it sets `next-error-last-buffer` to the current buffer.
This ensures that when `compilation-display-error` is called, it knows
which buffer to refer to for displaying the error."
  (interactive)
  (cscope-on-match
    (compilation-display-error)))

(defun cscope-focus-on-current-match-window ()
  "Focus on the window displaying the current match."
  (interactive)
  (cscope-on-match
    (compilation-display-error)
    (next-error 0)))

(defun cscope-current-match-buffer ()
  "Return the buffer associated with the current match."
  (let* ((loc (cscope-match-loc))
	 (file (caar (compilation--loc->file-struct loc)))
	 (path (concat (expand-file-name
			(concat default-directory "/" file)))))
    (cl-find path (buffer-list) :test 'string= :key 'buffer-file-name)))

(defun cscope-quit-current-match ()
  "Close the window displaying the current error if visible."
  (interactive)
  (cscope-on-match
    (when-let ((buffer (cscope-current-match-buffer))
	       (window (get-buffer-window buffer)))
      (with-selected-window window
	(quit-window)))))

(defun cscope-quit-all ()
  "Close all windows displaying cscope matches."
  (interactive)
  (for-all-cscope-match
    (cscope-quit-current-match)))

(defun cscope-filter-matches ()
  "Refreshes the cscope buffer, reapplying filters and highlighting."
  (cscope-print-filters)
  (if (cscope-search-tree-p)
      (cscope-execute-query)
    (cscope-refresh)))

(defun cscope-pop-filter ()
  "Remove the most recently applied filter from the cscope buffer.

This function removes the last filter that was applied to the cscope
results buffer, effectively undoing the last filtering operation. It
then refreshes the buffer to re-evaluate the remaining filters and
update the display accordingly. This allows users to progressively
remove filters to reveal more results."
  (interactive)
  (with-cscope-buffer
    (when cscope-filters
      (let ((regexp (car (last cscope-filters))))
	(setq cscope-filters (delete regexp cscope-filters))
	(cscope-filter-matches)))))

(defun cscope-kill-current-match-buffer ()
  "Kill the buffer of the current error."
  (interactive)
  (when-let ((buffer (cscope-current-match-buffer)))
    (kill-buffer buffer)))

(defun cscope-kill-all ()
  "Kill all buffers displaying cscope matches."
  (interactive)
  (for-all-cscope-match
    (cscope-kill-current-match-buffer)))

(defun cscope-filter-lines (regexp)
  "Filter cscope results in the current buffer using REGEXP.

With a prefix argument, *exclude* lines matching REGEXP.
Without a prefix argument, *include* only lines matching REGEXP.

The REGEXP is read from the minibuffer, and added to
`cscope-filter-history' for easy reuse.  A prefix argument inverts
the filtering behavior, deleting lines that *match* the regexp instead
of those that don't.  The buffer is modified in place."
  (interactive (list (read-string (format "Filter%s regular expression: "
					  (if current-prefix-arg
					      " out"
					    ""))
				  nil 'cscope-filter-history)))
  (with-cscope-buffer
    (let ((elem (propertize regexp
			    'cscope-filter-out current-prefix-arg)))
      (add-to-list 'cscope-filters elem t))
    (cscope-filter-matches)
    (message (concat (propertize regexp 'face 'match)
		     (substitute-command-keys " filter applied, pop filters\
 using the \\[cscope-pop-filter] key.")))))

;; TODO: Override tree-widget-button-click

(defun cscope-tree-toggle ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (while (and (not (get-char-property (point) 'button))
		(not (= (point) (line-end-position))))
      (forward-char))
    (when (get-char-property (point) 'button)
      (let ((node (widget-get (get-char-property (point) 'button) :parent)))
	(if (not (widget-get node :args))
	    (cscope-tree-build-childs node)
	  (widget-button-press (point)))))))

(defun cscope-previous-match ()
  "Move to the previous cscope match."
  (interactive)
  (if (cscope-search-tree-p)
      (widget-backward 1)
    (compilation-previous-error 1)))

(defun cscope-next-match ()
  "Move to the next cscope match."
  (interactive)
  (when (< (line-beginning-position) (cscope-last-error-position))
    (if (cscope-search-tree-p)
	(widget-forward 1)
      (compilation-next-error 1))))

(defun cscope-kill-compilation ()
  "Stop the running cscope process and clear related state."
  (interactive)
  (let ((buffer (current-buffer)))
    (while (process-live-p (get-buffer-process buffer))
      (when-let ((process (get-buffer-process buffer)))
	(set-process-filter process #'ignore)
	(set-process-sentinel process #'ignore)
	(interrupt-process process)
	(setq cscope-lock nil
	      cscope-tree-requests nil))))
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun cscope-tree-up ()
  (interactive)
  (goto-char (line-beginning-position))
  (while (and (not (get-char-property (point) 'button))
	      (not (= (point) (line-end-position))))
    (forward-char))
  (when-let* ((node (widget-get (get-char-property (point) 'button) :parent))
	      (parent (widget-get node :parent)))
    (goto-char (widget-get parent :from))))

(defvar cscope-mode-map (cl-copy-list grep-mode-map))
(define-key cscope-mode-map (kbd "<return>") #'cscope-goto-match)
(define-key cscope-mode-map (kbd "<tab>") #'cscope-next-match)
(define-key cscope-mode-map (kbd "S-<tab>") #'cscope-previous-match)
(define-key cscope-mode-map (kbd "C-o") nil)
(define-key cscope-mode-map (kbd "C-c C-k") #'cscope-kill-compilation)
(define-key cscope-mode-map (kbd "e") #'cscope-entry)
(define-key cscope-mode-map (kbd "f") #'cscope-filter-lines)
(define-key cscope-mode-map (kbd "F") #'cscope-pop-filter)
(define-key cscope-mode-map (kbd "g") #'cscope-re-execute-query)
(define-key cscope-mode-map (kbd "G") #'cscope-generate-database)
(define-key cscope-mode-map (kbd "k") #'cscope-kill-current-match-buffer)
(define-key cscope-mode-map (kbd "K") #'cscope-kill-all)
(define-key cscope-mode-map (kbd "o") #'cscope-display-match)
(define-key cscope-mode-map (kbd "O") #'cscope-focus-on-current-match-window)
(define-key cscope-mode-map (kbd "q") #'cscope-quit-current-match)
(define-key cscope-mode-map (kbd "Q") #'cscope-quit-all)
(define-key cscope-mode-map (kbd "t") #'cscope-toggle)
(define-key cscope-mode-map (kbd "N") #'cscope-next-query)
(define-key cscope-mode-map (kbd "P") #'cscope-previous-query)
(define-key cscope-mode-map (kbd "u") #'cscope-tree-up)
(define-key cscope-mode-map (kbd "x") #'cscope-tree-toggle)

(defun cscope-first-error-position ()
  "Return the position of the first visible cscope match in the buffer.

This function searches from the beginning of the cscope results buffer
(after the header lines) and returns the position of the first line
that is not marked as invisible by a filter.  If all lines are filtered
out, it returns the end of the buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (and (not (eobp)) (cscope-invisible-match))
      (forward-line))
    (point)))

(defun cscope-last-error-position ()
  "Return the position of the last visible cscope match in the buffer.

This function searches backward from the end of the cscope results
buffer and returns the position of the last line that is not marked
as invisible by a filter.  If all lines are filtered out, it returns
the beginning of the buffer (after the header lines)."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (while (and (not (= 2 (line-number-at-pos))) (cscope-invisible-match))
      (forward-line -1))
    (point)))

(defun cscope-next-error-function (arg &optional reset)
  "Navigate to the next or previous visible cscope match.

ARG is a number indicating how many matches to move forward (positive)
or backward (negative).

RESET, if non-nil, forces the navigation to start from the first
visible match in the buffer.

This function respects filters applied to the cscope buffer, only
stopping at lines that are not marked as invisible."
  (with-cscope-buffer
    (when reset
      (goto-char (cscope-first-error-position)))
    (let ((inc (if (> arg 0) 1 -1))
	  (limit (if (> arg 0)
		     (cscope-last-error-position)
		   (cscope-first-error-position))))
      (let ((i 0))
	(while (and (not (= (point) limit))
		    (< i (abs arg)))
	  (forward-line inc)
	  (unless (cscope-invisible-match)
	    (cl-incf i))))
      (let ((cmp (if (> arg 0) '> '<)))
	(when (funcall cmp (point) limit)
	  (goto-char limit))))
    (setq compilation-current-error (point-marker)))
  (compilation-next-error-function 0))

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
    (let* ((regexp-alist (copy-list grep-regexp-alist)))
      (setf (caar regexp-alist)
	    "\\(?:\\(?1:[a-zA-Z0-9_<>/\.\-]+\\):\\(?2:[0-9]+\\)\\)")
      (setq-local compilation-error-regexp-alist regexp-alist
		  compilation-error-face grep-hit-face
		  compilation-mode-line-errors mode-line
		  next-error-function #'cscope-next-error-function
		  tree-widget-after-toggle-functions '(cscope-tree-widget-after-toggle)))
    (cscope--init)))

;; Make sure function and menu are initialized on load
(cscope--init)
(provide 'cscope)
