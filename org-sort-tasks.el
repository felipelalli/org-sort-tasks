;;; org-sort-tasks.el --- An easy way to sort your long TODO list.  -*- lexical-binding: t -*-

;; Version: 2.0
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

(defun sort-tasks/sort/interactive (task1-description task2-description)
  (y-or-n-p (format "Should:\n...'%s'\nbe done *BEFORE*\n...'%s'?"
                    task2-description
                    task1-description)))

(defun sort-tasks/sort (task1 task2)
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
          (t (not (with-local-quit
                    (sort-tasks/sort/interactive
                     (car (org-element-property :title task1))
                     (car (org-element-property :title task2)))))))))

;;
;; SORT A LIST OF TASKS
;;

(defun sort-tasks/sort-list (task-list)
  (sort task-list 'sort-tasks/sort))

(defun sort-tasks/sort-children (final-buffer element)
  "This fn receives a root element and sort all its children.

Note: sort-tasks/sort-children is private and it is used by the main org-sort-tasks fn."
  (let* ((list-of-tasks
          (org-element-map element 'headline
            (lambda (task)
              (if (and (= (+ (org-element-property :level element) 1)
                          (org-element-property :level task)))
                  task
                nil))))
         (aprox-steps (ceiling (* (length list-of-tasks) (log (max 1 (length list-of-tasks)) 5)))))
    (let ((sorted-list (sort-tasks/sort-list list-of-tasks)))
      (with-current-buffer final-buffer (insert (format "* %s\n" (car (org-element-property :title element)))))
      (mapcar (lambda (c)
                (let ((task-content (buffer-substring (org-element-property :begin c)
                                                      (org-element-property :end c))))
                  (with-current-buffer final-buffer
                    (insert (format "%s" task-content)))))
              sorted-list)
      t)))

(defun org-sort-tasks/main ()
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
                       (sort-tasks/sort-children final-buffer task))))))
            (if (null result-list)
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

1) Let the cursor at any position of a root headline and press M-x org-sort-tasks.
2) Mark a region and use M-x org-sort-tasks.

The user will be prompted to reply a simple question like \"Should 'xxx task' BE DONE BEFORE 'yyy task'?\". After reply some questions, the fn will open a new buffer and build a sorted list of tasks. It is very useful for who uses GTD method and work with huge unsorted lists of tasks. The number of questions will be in avg O(n log n)."
  (interactive)
  (org-sort-tasks/main))

;;
;; INSERT A NEW TASK
;;

(defun org-insert-sorted-todo-heading/insert (position before)
  (goto-char position)
  (beginning-of-line)
  (if before
      (org-insert-todo-heading nil)
      (org-insert-todo-heading-respect-content))
  (message "Done!"))

(defun org-insert-sorted-todo-heading/insert-in-right-position-using-binary-search (task-list)
  (when (< (length task-list) 1)
    (error "The list is empty."))
  (cond ((= (length task-list) 1)
         (org-insert-sorted-todo-heading/insert
          (org-element-property :begin (car task-list))
          (sort-tasks/sort/interactive
              (car (org-element-property :title (car task-list)))
              "THE NEW TASK")))
        (t (let* ((pivot (/ (length task-list) 2))
                  (left-list (butlast task-list (- (length task-list) pivot)))
                  (right-list (nthcdr (+ pivot 1) task-list)))
             (if (sort-tasks/sort/interactive
                  (car (org-element-property :title (nth pivot task-list)))
                  "THE NEW TASK")
                 (org-insert-sorted-todo-heading/insert-in-right-position-using-binary-search
                  left-list)
               (if (null right-list)
                   (org-insert-sorted-todo-heading/insert (org-element-property :begin (nth pivot task-list)) nil)
                   (org-insert-sorted-todo-heading/insert-in-right-position-using-binary-search
                    right-list)))))))

(defun org-insert-sorted-todo-heading/main ()
  (if (use-region-p)
      (error "Do not mark. Just let the cursor at some root heading.")
      (progn
        (beginning-of-line)
        (org-mark-subtree)
        (next-line)
        (deactivate-mark)        
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (beginning-of-buffer)
          (let ((first-element (org-element-at-point)))
            (if (not (eq (org-element-type first-element) 'headline))
                (error "The first element must be a headline.")
                (org-insert-sorted-todo-heading/insert-in-right-position-using-binary-search
                  (org-element-contents (org-element-parse-buffer))))))
        (recenter-top-bottom))))

(defun org-insert-sorted-todo-heading ()
  "An interactive fn that inserts a TODO heading in the right position in a pre-sorted list. Let the cursor above the root (parent) element.

*WARNING:* If the list is unsorted, use `org-sort-tasks` first."
  (interactive)
  (org-insert-sorted-todo-heading/main))

;; Export

(provide 'org-sort-tasks)
(provide 'org-insert-sorted-todo-heading)
