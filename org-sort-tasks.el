;;; org-sort-tasks.el --- An easy way to sort your long TODO list.  -*- lexical-binding: t -*-

;; Version: 1.1
;; Keywords: orgmode, sort, task, todo, ordered list

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

(defun sort-tasks/timestamp-obj=? (ts1 ts2)
  "Compare two timestamp object and returns true if they are equal until day level."
  (and (= (org-element-property :year-start ts1)
          (org-element-property :year-start ts2))
       (= (org-element-property :month-start ts1)
          (org-element-property :month-start ts2))
       (= (org-element-property :day-start ts1)
          (org-element-property :day-start ts2))))

(defun sort-tasks/timestamp-obj<? (ts1 ts2)
  "Compare two timestamp object and returns true if ts1 is early than ts2 until day level."
  (or (< (org-element-property :year-start ts1)
         (org-element-property :year-start ts2))
      (and (= (org-element-property :year-start ts1)
              (org-element-property :year-start ts2))
           (< (org-element-property :month-start ts1)
              (org-element-property :month-start ts2)))
      (and (= (org-element-property :year-start ts1)
              (org-element-property :year-start ts2))
           (= (org-element-property :month-start ts1)
              (org-element-property :month-start ts2))
           (< (org-element-property :day-start ts1)
              (org-element-property :day-start ts2)))))

(defun sort-tasks/sort (task1 task2 do-not-ask-to-user)
  "Decides if task1 should be done before task2 or not. First, look to deadline, scheduled, priority and then ask to the user."
  (let ((t1 (or (org-element-property :deadline task1) (org-element-property :scheduled task1)))
        (t2 (or (org-element-property :deadline task2) (org-element-property :scheduled task2)))
        (p1 (or (org-element-property :priority task1) org-default-priority))
        (p2 (or (org-element-property :priority task2) org-default-priority)))
    (cond ((eq (org-element-property :todo-type task1) 'done) nil)
          ((eq (org-element-property :todo-type task2) 'done) t)
          ((and t1 (not t2)) t)
          ((and t2 (not t1)) nil)
          ((and t1 t2
                (not (sort-tasks/timestamp-obj=? t1 t2)))
           (if (sort-tasks/timestamp-obj<? t1 t2) t nil))
          ((< p1 p2) t)
          ((> p1 p2) nil)
          (do-not-ask-to-user t)
          (t (not (with-local-quit
		    (y-or-n-p (format "Should:\n...'%s'\nbe done *BEFORE*\n...'%s'?"
                                      (car (org-element-property :title task2))
                                      (car (org-element-property :title task1))))))))))

(defun sort-tasks/sort-list (task-list tolerance roughly)
  "This fn receives a list of tasks. Each task is a vector composed by the task element itself and its raw content.

Note: sort-tasks/sort-list is private and is used by sort-tasks/sort-children"
  (let ((sorted-list
         (cond ((<= (length task-list) 1) task-list)
               ((= (length task-list) 2)
                (if (sort-tasks/sort
                     (aref (nth 0 task-list) 0)
                     (aref (nth 1 task-list) 0)
                     (<= (length task-list) tolerance))
                    task-list
                  (reverse task-list)))
               (t (let ((pivot (/ (length task-list) 2))
                        (left-list '())
                        (right-list '()))
                    (cl-labels ((go-next (c)
                                         (cond ((= c pivot)
                                                (go-next (+ c 1)))
                                               ((>= c (length task-list))
                                                (list (sort-tasks/sort-list left-list tolerance roughly)
                                                      (nth pivot task-list)
                                                      (if (or (not roughly)
							      (<= (length right-list) 3))
                                                          (sort-tasks/sort-list right-list tolerance roughly)
                                                          right-list)
                                                      ))
                                               (t (progn
                                                    (if (sort-tasks/sort
                                                         (aref (nth c task-list) 0)
                                                         (aref (nth pivot task-list) 0)
                                                         (<= (length task-list) tolerance))
                                                        (setq left-list (cons (nth c task-list) left-list))
                                                      (setq right-list (cons (nth c task-list) right-list)))
                                                    (go-next (+ c 1)))))))
                      (go-next 0)))))))
    (sort-tasks/flatten sorted-list)))

(defun sort-tasks/sort-children (final-buffer element tolerance roughly)
  "This fn receives a root element and sort all its children.

Note: sort-tasks/sort-children is private and it is used by the main org-sort-tasks fn."
  (let* ((list-of-tasks
          (org-element-map element 'headline
            (lambda (task)
              (if (and (= (+ (org-element-property :level element) 1)
                          (org-element-property :level task)))
                  (vector task
                          (buffer-substring (org-element-property :begin task)
                                            (org-element-property :end task)))
                nil))))
         (aprox-steps (ceiling (* (length list-of-tasks) (log (max 1 (length list-of-tasks)) 5)))))
    (let ((sorted-list (sort-tasks/sort-list list-of-tasks tolerance roughly)))
      (with-current-buffer final-buffer
        (insert (format "* %s\n" (car (org-element-property :title element))))
        (mapcar (lambda (c)
                  (insert (format "%s" (aref c 1))))
                sorted-list)
        t))))

(defun org-sort-tasks/main (tolerance roughly)
  (cl-assert (>= tolerance 1) t "tolerance should be >= 1: %d")
  (let ((final-buffer (generate-new-buffer "*sorted-tasks*"))
        (no-selection (not (use-region-p)))
	(inhibit-quit t)) ; If C-g is pressed then try to build a partial sorted list.
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
          (let ((result-list
                 (org-element-map (org-element-parse-buffer) 'headline
                   (lambda (task)
                     (when (= (org-element-property :level first-element)
                              (org-element-property :level task))
                       (sort-tasks/sort-children final-buffer task tolerance roughly))))))
            (if (= (length result-list) 0)
                (message "Aborted.")
                (progn
                  (switch-to-buffer final-buffer)
                  (beginning-of-buffer)
                  (org-mode)
                  (org-cycle)
                  (message "Done! A sorted list was built and opened in a new disposable buffer.")))))))))

(defun org-sort-tasks ()
  "An interactive fn that sorts a list of tasks in the selected region or under the headline on cursor.

There are two main ways of use:

1) You can let the cursor above any position of a headline and press M-x org-sort-tasks.
2) You can select a region and use M-x org-sort-tasks.

The user will be prompted to reply a simple question like \"Should 'xxx task' BE DONE BEFORE 'yyy task'?\". After reply some questions, the fn will open a new buffer and build a sorted list of tasks. It is very useful for who uses GTD method and work with huge unsorted lists of tasks.

See also:

- org-sort-tasks-roughly
"
  (interactive)
  (org-sort-tasks/main 1 nil))

(defun org-sort-tasks-roughly ()
  "The same of `org-sort-tasks` but it tends to make the top of the list sorted and the rest messy. In the sort algorithm, it always leaves the second half unsorted."
  (interactive)
  (org-sort-tasks/main 1 t))

;; Export

(provide 'org-sort-tasks)
(provide 'org-sort-tasks-roughly)
; org-fix-task-position
