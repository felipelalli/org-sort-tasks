;;; org-sort-tasks.el --- An easy way to sort your long TODO list.  -*- lexical-binding: t -*-

;; Version: 1.0
;; Keywords: orgmode, sort, task, todo, ordered list, merge sort

;; MIT License

;; Copyright (c) 2019 Felipe Micaroni Lalli

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; ---------------------------------------------------------------------------------

;; External functions copied from Dash Library (by Magnar Sveen <magnars@gmail.com>):

;;
;; From https://github.com/magnars/dash.el
;;
(defmacro sort-tasks/--map (form list)
  "Anaphoric form of `-map'."
  (declare (debug (form form)))
  `(mapcar (lambda (it) ,form) ,list))

;;
;; From https://github.com/magnars/dash.el
;;
(defmacro sort-tasks/--mapcat (form list)
  "Anaphoric form of `-mapcat'."
  (declare (debug (form form)))
  `(apply 'append (sort-tasks/--map ,form ,list)))

;;
;; From https://github.com/magnars/dash.el
;;
(defun sort-tasks/-mapcat (fn list)
  "Return the concatenation of the result of mapping FN over LIST.
Thus function FN should return a list."
  (sort-tasks/--mapcat (funcall fn it) list))

;;
;; From https://github.com/magnars/dash.el
;;
(defun sort-tasks/flatten (l)
  "Take a nested list L and return its contents as a single, flat list.
Note that because `nil' represents a list of zero elements (an
empty list), any mention of nil in L will disappear after
flattening.  If you need to preserve nils, consider `-flatten-n'
or map them to some unique symbol and then map them back.
Conses of two atoms are considered \"terminals\", that is, they
aren't flattened further.
See also: `-flatten-n'"
  (declare (pure t) (side-effect-free t))
  (if (and (listp l) (listp (cdr l)))
      (-mapcat 'sort-tasks/flatten l)
    (list l)))

;; ---------------------------------------------------------------------------------

;; Code:

(defun sort-tasks/sort-list (task-list)
  "This fn receives a list of tasks. Each task is a vector composed by the title of task and its raw content. It uses the merge sort technique to sort the list asking to the user which task should be done before another task.

Note: sort-tasks/sort-list is private and is used by sort-tasks/sort-children"
  (let ((sorted-list
	 (cond ((<= (length task-list) 1) task-list)
	       ((= (length task-list) 2)
		(if (y-or-n-p (format "'%s' *SHOULD BE DONE BEFORE* '%s'?"
				      (aref (nth 0 task-list) 0)
				      (aref (nth 1 task-list) 0)))
		    task-list
		  (reverse task-list)))
	       (t (let ((pivot (/ (length task-list) 2))
			(left-list '())
			(right-list '()))
		    (cl-labels ((go-next (c)
					 (cond ((= c pivot)
						(go-next (+ c 1)))
					       ((>= c (length task-list))
						(list (sort-tasks/sort-list left-list)
						      (nth pivot task-list)
						      (sort-tasks/sort-list right-list)))
					       (t (progn
						    (if (y-or-n-p (format "'%s' *SHOULD BE DONE BEFORE* '%s'?"
									  (aref (nth c task-list) 0)
									  (aref (nth pivot task-list) 0)))
							(setq left-list (cons (nth c task-list) left-list))
						      (setq right-list (cons (nth c task-list) right-list)))
						    (go-next (+ c 1)))))))
		      (go-next 0)))))))
    (sort-tasks/flatten sorted-list)))

(defun sort-tasks/sort-children (final-buffer element)
  "This fn receives a root element and sort all its children.

Note: sort-tasks/sort-children is private and it is used by the main org-sort-tasks fn."
  (let* ((list-of-tasks
	 (org-element-map element 'headline
	   (lambda (task)
	     (if (and (= (+ (org-element-property :level element) 1)
			 (org-element-property :level task))
		      (eq (org-element-property :todo-type task)
			  'todo))
		 (vector (car (org-element-property :title task))
			 (buffer-substring (org-element-property :begin task)
					   (org-element-property :end task)))
	       nil))))
	 (sorted-list (sort-tasks/sort-list list-of-tasks)))
    (with-current-buffer final-buffer
      (insert (format "* %s\n" (car (org-element-property :title element))))
      (mapcar (lambda (c)
		(insert (format "%s" (aref c 1))))
	      sorted-list)
      (beginning-of-buffer)
      (org-mode)
      (org-cycle)
      (message "Done! A sorted list was built and opened in a new disposable buffer.")
      )))

;;
;; MAIN INTERACTIVE FUNCTION "org-sort-tasks".
;;
(defun org-sort-tasks ()
  "An interactive fn that sorts a list of tasks in the selected region or under the headline on cursor.

There are two main ways of use:

1) You can let the cursor above any position of a headline and press M-x org-sort-tasks.
2) You can select a region and use M-x org-sort-tasks.

The user will be prompted to reply a simple question like \"Is 'xxx task' SHOULD BE DONE BEFORE 'yyy task'?\". After reply some questions, the fn will open a new buffer and build a sorted list of tasks. It is very useful for who uses GTD method and work with huge unsorted lists of tasks."
  (interactive)
  (let ((final-buffer (generate-new-buffer "*sorted-tasks*"))
	(no-selection (not (use-region-p))))
    (with-current-buffer final-buffer (erase-buffer))
    (when no-selection
      (beginning-of-line)
      (org-mark-subtree))
    (deactivate-mark)
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (beginning-of-buffer)
      (let ((first-element (org-element-at-point)))
	(if (not (eq (org-element-type first-element) 'headline))
	    (error "The first element must be a headline.")
	  (progn
	    (org-element-map (org-element-parse-buffer) 'headline
	      (lambda (task)
		(when (= (org-element-property :level first-element)
			 (org-element-property :level task))
		  (sort-tasks/sort-children final-buffer task))))
	    (switch-to-buffer final-buffer)))))))

(provide 'org-sort-tasks)
