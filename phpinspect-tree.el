;;; phpinspect-buffer.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(cl-defstruct (phpinspect-tree (:constructor phpinspect-make-tree))
  "An n-ary tree implementation to store integer intervals.

Nodes within a layer of the tree are not allowed to overlap each
other. Trying to add an overlapping node that cannot enclose or
be enclosed in an existing node will result in an error.

Each node can have an infinite number of child nodes.

It is advisable for performance to define a root node with a
range that encloses all nodes that are going to be added, as this
will limit the amount of pointer shuffling to keep the root node
reference intact (see also `phininspect-tree-insert-node'). That
being said, not doing so will not limit the trees
functionalities."
  (parent nil
          :type phpinspect-tree)
  (children (phpinspect-make-ll)
            :type phpinspect-llnode)
  (start 0
         :type integer)
  (end 0
       :type integer)
  (value nil))

(cl-defstruct (phpinspect-llnode (:constructor phpinspect-make-ll))
  "A linked list implementation.

Links for specific cells are tracked and can be looked up via the
link-map. This does assume that no duplicate cells are inserted
however (multiple cells that are `eq' to each other). If
duplicate cells are inserted, only the last inserted duplicate
can be looked up via the link-map.

A few generic sequence functions have been implemented. Some of
these, like `seq-take-while' return an instance of
`phpinspect-slice', which is a window into a subsection of the
list it was called on."
  (left nil
        :type phpinspect-llnode)
  (right nil
         :type phpinspect-llnode)
  (link-map (make-hash-table :test #'eq :size 100 :rehash-size 400)
            :type hash-table
            :documentation
            "Table to lookup the links in which values are stored.")
  (value nil))

(cl-defstruct (phpinspect-slice (:constructor phpinspect-make-slice))
  "A window to a subsection of a (`phpinspect-llnode') linked list. "
  (reversed nil
            :type bool
            :documentation
            "Whether the slice should be iterated in reverse")
  (start nil)
  (end nil))

(defmacro phpinspect-doslice (place-and-slice &rest body)
  (declare (indent defun))
  (let ((list (gensym))
        (slice-end (gensym))
        (normal-next-function #'phpinspect-llnode-right)
        (reverse-next-function #'phpinspect-llnode-left))

    `(if (phpinspect-slice-reversed ,(cadr place-and-slice))
         (let ((,list (phpinspect-slice-end ,(cadr place-and-slice)))
               (,slice-end (phpinspect-llnode-left
                            (phpinspect-slice-start ,(cadr place-and-slice)))))
           (when (phpinspect-llnode-value ,list)
             (while (and ,list (not (eq ,slice-end ,list)))
               (let ((,(car place-and-slice) (phpinspect-llnode-value ,list)))
                 ,@body)
               (setq ,list (,reverse-next-function ,list)))))

       (let ((,list (phpinspect-slice-start ,(cadr place-and-slice)))
             (,slice-end (phpinspect-llnode-right
                          (phpinspect-slice-end ,(cadr place-and-slice)))))
         (when (phpinspect-llnode-value ,list)
           (while (and ,list (not (eq ,slice-end ,list)))
             (let ((,(car place-and-slice) (phpinspect-llnode-value ,list)))
               ,@body)
             (setq ,list (,normal-next-function ,list))))))))

(cl-defmethod seq-reverse ((slice phpinspect-slice))
  (setf (phpinspect-slice-reversed slice) (not (phpinspect-slice-reversed slice)))
  slice)

(cl-defmethod phpinspect-slice-detach ((slice phpinspect-slice))
  "Detach underlying link range from the linked list that it
belongs to. Return resulting linked list."
  (let* ((start (phpinspect-slice-start slice))
         (end (phpinspect-slice-end slice))
         (right-neighbour (phpinspect-llnode-right end))
         (left-neighbour (phpinspect-llnode-left start)))
    ;; No left-linked node means that `start' is the root reference to the
    ;; list. This cannot be detached, so we need to create a new link that will
    ;; serve as root for the detached list.
    (unless left-neighbour
      (let ((new-start (phpinspect-make-ll :right (phpinspect-llnode-right start)
                                           :value (phpinspect-llnode-value start)
                                           :link-map (phpinspect-llnode-link-map start))))
        (when (phpinspect-llnode-right start)
          (setf (phpinspect-llnode-left (phpinspect-llnode-right start)) new-start))

        ;; Make right neighbour the new root node of the list
        (if right-neighbour
            (progn
              (phpinspect-ll-relink start (phpinspect-llnode-value right-neighbour))
              (setq right-neighbour (phpinspect-llnode-right right-neighbour)))
          (phpinspect-ll-relink start nil))

        (setq left-neighbour start)
        (when (eq start end) (setq end new-start))
        (setq start new-start)

        ;; Update slice itself
        (setf (phpinspect-slice-start slice) start)
        (setf (phpinspect-slice-end slice) end)))

    (if (eq start end)
        start
      (when right-neighbour
        (setf (phpinspect-llnode-left right-neighbour) left-neighbour))
      (setf (phpinspect-llnode-right left-neighbour) right-neighbour)

      (setf (phpinspect-llnode-left start) nil)
      (setf (phpinspect-llnode-right end) nil)

      ;; Fix broken references in old link-map and create separate link-map for
      ;; the new detached list.
      (let ((list start)
            (link-map (make-hash-table :test #'eq :size 100 :rehash-size 400)))
        (while list
          (phpinspect-ll-unregister-link list)
          (setf (phpinspect-llnode-link-map list) link-map)
          (phpinspect-ll-register-link list)
          (setq list (phpinspect-llnode-right list))))

      start)))

(cl-defmethod phpinspect-llnode-detach ((list phpinspect-llnode))
  (let ((left (phpinspect-llnode-left list))
        (right (phpinspect-llnode-right list)))
    (when left (setf (phpinspect-llnode-right left) right))
    (when right (setf (phpinspect-llnode-left right) left))
    (phpinspect-ll-unregister-link list)

    list))

(cl-defmethod phpinspect-ll-register-link ((list phpinspect-llnode))
  (puthash (phpinspect-llnode-value list) list (phpinspect-llnode-link-map list)))

(cl-defmethod phpinspect-ll-unregister-link ((list phpinspect-llnode))
  (remhash (phpinspect-llnode-value list) (phpinspect-llnode-link-map list)))

(cl-defmethod phpinspect-ll-first ((list phpinspect-llnode))
  (while (phpinspect-llnode-left list)
    (setq list (phpinspect-llnode-left list)))
  (or (phpinspect-llnode-left list) list))

(cl-defmethod phpinspect-ll-last ((list phpinspect-llnode))
  (while (phpinspect-llnode-right list)
    (setq list (phpinspect-llnode-right list)))
  (or (phpinspect-llnode-right list) list))

(cl-defmethod phpinspect-ll-link ((list phpinspect-llnode) value)
  (or (gethash value (phpinspect-llnode-link-map list))
      (catch 'found
        (while list
          (when (eq value (phpinspect-llnode-value list))
            (phpinspect-ll-register-link list)
            (throw 'found list))
          (setq list (phpinspect-llnode-right list))))))

(cl-defmethod phpinspect-ll-relink ((list phpinspect-llnode) value)
  (phpinspect-ll-unregister-link list)
  (setf (phpinspect-llnode-value list) value)
  (phpinspect-ll-register-link list))

(cl-defmethod phpinspect-ll-push (value (list phpinspect-llnode))
  (setq list (phpinspect-ll-first list))

  (if (phpinspect-llnode-value list)
      (let* ((old-right (phpinspect-llnode-right list))
             (new-right (phpinspect-make-ll
                         :left list
                         :link-map (phpinspect-llnode-link-map list)
                         :value (phpinspect-llnode-value list)
                         :right old-right)))
        (phpinspect-ll-register-link new-right)
        (setf (phpinspect-llnode-value list) value)
        (setf (phpinspect-llnode-right list) new-right)
        (phpinspect-ll-register-link list)
        (when old-right
          (setf (phpinspect-llnode-left old-right) new-right)))
    ;; else
    (setf (phpinspect-llnode-value list) value)
    (phpinspect-ll-register-link list))
  list)

(cl-defmethod phpinspect-ll-insert-right ((list phpinspect-llnode) value)
  (let* ((original-right (phpinspect-llnode-right list))
         (new-link (phpinspect-make-ll :left list
                                       :link-map (phpinspect-llnode-link-map list)
                                       :right original-right
                                       :value value)))
    (phpinspect-ll-register-link new-link)
    (setf (phpinspect-llnode-right list) new-link)
    (when original-right
      (setf (phpinspect-llnode-left original-right) new-link))))

(cl-defmethod phpinspect-ll-insert-left ((list phpinspect-llnode) value)
  (let* ((original-left (phpinspect-llnode-left list))
         (new-link (phpinspect-make-ll :right list
                                       :link-map (phpinspect-llnode-link-map list)
                                       :left original-left
                                       :value value)))
    (phpinspect-ll-register-link new-link)
    (setf (phpinspect-llnode-left list) new-link)
    (when original-left
      (setf (phpinspect-llnode-right original-left) new-link))))

(cl-defmethod seq-elt ((list phpinspect-llnode) (n integer))
  (setq list (phpinspect-ll-first list))

  (let ((current-elt 0))
    (while (and list (not (= current-elt n)))
      (setq list (phpinspect-llnode-right list)
            current-elt (+ current-elt 1)))

    (when list
      (phpinspect-llnode-value list))))

(cl-defmethod seq-elt ((slice phpinspect-slice) (n integer))
  (let ((list (phpinspect-slice-start slice))
        (end (phpinspect-llnode-right (phpinspect-slice-end slice))))

  (let ((current-elt 0))
    (while (and list (not (= current-elt n)))
      (setq list (phpinspect-llnode-right list)
            current-elt (+ current-elt 1)))
      (when (eq end list)
        (setq list nil)))

  (when list
    (phpinspect-llnode-value list))))

(cl-defmethod seq-do (fn (list phpinspect-llnode))
  (when (phpinspect-llnode-value list)
    (while list
      (funcall fn (phpinspect-llnode-value list))
      (setq list (phpinspect-llnode-right list)))))

(cl-defmethod seq-do (fn (slice phpinspect-slice))
  (phpinspect-doslice (val slice)
    (funcall fn val)))

(cl-defmethod seq-take-while (pred (list phpinspect-llnode))
  (when (phpinspect-llnode-value list)
    (let ((start list)
          (end list))
      (while (and list (funcall pred (phpinspect-llnode-value list)))
        (setq end list)
        (setq list (phpinspect-llnode-right list)))

      (phpinspect-make-slice :start start :end end))))

(cl-defmethod seq-take-while (pred (slice phpinspect-slice))
  (let ((start (phpinspect-slice-start slice))
        (end (phpinspect-slice-start slice)))
  (catch 'break
    (phpinspect-doslice (val slice)
      (if (funcall pred val)
          (setq end (phpinspect-ll-link start val))
        (throw 'break nil))))

  (phpinspect-make-slice :start start :end end)))

(cl-defmethod seq-length ((list phpinspect-llnode))
  (let ((count 0))
    (while (and list (phpinspect-llnode-value list))
      (setq count (+ 1 count)
            list (phpinspect-llnode-right list)))
    count))

(cl-defmethod seq-length ((slice phpinspect-slice))
  (let ((count 0)
        (list (phpinspect-slice-start slice))
        (end (phpinspect-llnode-right (phpinspect-slice-end slice))))
    (while (and list (not (eq end list))
                (phpinspect-llnode-value list))
      (setq count (+ 1 count)
            list (phpinspect-llnode-right list))
      (when (eq end list)
        (setq list nil)))

    count))

(cl-defmethod seq-into ((list phpinspect-llnode) type)
  (if (eq 'slice type)
      (phpinspect-make-slice :start list :end (phpinspect-ll-last list))
    (let ((destination)
          (list (phpinspect-ll-last list)))
      (while list
        (push (phpinspect-llnode-value list) destination)
        (setq list (phpinspect-llnode-left list)))

      (cond ((eq 'vector type) (vconcat destination))
            ((eq 'list type) destination)
            ((eq 'string type) (concat destination))
            (t (error "Not a sequence type name: %S" type))))))

(cl-defmethod seq-into ((slice phpinspect-slice) type)
    (let ((destination))
      (unwind-protect
          (progn
            (seq-reverse slice)
            (phpinspect-doslice (val slice)
              (push val destination)))
        (seq-reverse slice))

      (cond ((eq 'vector type) (vconcat destination))
            ((eq 'list type) destination)
            ((eq 'string type) (concat destination))
            (t (error "Not a sequence type name: %S" type)))))

(cl-defmethod seq-find (pred (list phpinspect-llnode) &optional default)
  (if (phpinspect-llnode-value list)
    (while (and list (not (funcall pred (phpinspect-llnode-value list))))
      (setq list (phpinspect-llnode-right list)))
    (setq list nil))

  (if list
      (phpinspect-llnode-value list)
    default))

(cl-defmethod seq-find (pred (slice phpinspect-slice) &optional default)
  (or
   (catch 'found
     (phpinspect-doslice (val slice)
       (when (funcall pred val)
         (throw 'found val))))
   default))

(cl-defmethod phpinspect-ll-pp ((list phpinspect-llnode))
  (message "(phpinspect-ll %s)"
           (string-join (seq-map (lambda (x) (format "%s" x)) list) ", ")))

(cl-defmethod phpinspect-llnode-is-tail ((list phpinspect-llnode))
  (not (phpinspect-llnode-right list)))

(cl-defmethod seq-empty-p ((list phpinspect-llnode))
  (and (not (phpinspect-llnode-value list))
       (phpinspect-llnode-is-tail list)))

(cl-defmethod phpinspect-tree-overlaps ((tree phpinspect-tree) (point integer))
  (and (> (phpinspect-tree-end tree) point)
       (<= (phpinspect-tree-start tree) point)))

(cl-defmethod phpinspect-tree-overlaps ((tree1 phpinspect-tree) (tree2 phpinspect-tree))
  (or (phpinspect-tree-overlaps tree1 (phpinspect-tree-start tree2))
      (phpinspect-tree-overlaps tree1 (- (phpinspect-tree-end tree2) 1))
      (phpinspect-tree-overlaps tree2 (phpinspect-tree-start tree1))
      (phpinspect-tree-overlaps tree2 (- (phpinspect-tree-end tree1) 1))))

(cl-defmethod phpinspect-tree-overlaps ((tree phpinspect-tree) region)
  (or (phpinspect-tree-overlaps tree (phpinspect-region-start region))
      (phpinspect-tree-overlaps tree (- (phpinspect-region-end region) 1))
      (phpinspect-region-overlaps-point region (phpinspect-tree-start tree))
      (phpinspect-region-overlaps-point region (- (phpinspect-tree-end tree) 1))))

(cl-defmethod phpinspect-tree-starts-after ((tree phpinspect-tree) (point integer))
  (> (phpinspect-tree-start tree) point))

(cl-defmethod phpinspect-tree-encloses ((tree1 phpinspect-tree) (tree2 phpinspect-tree))
  (and (<= (phpinspect-tree-start tree1) (phpinspect-tree-start tree2))
       (>= (phpinspect-tree-end tree1) (phpinspect-tree-end tree2))))

(cl-defmethod phpinspect-tree-switch-attributes ((tree1 phpinspect-tree) (tree2 phpinspect-tree))
  (let ((parent (phpinspect-tree-parent tree1))
        (children (phpinspect-tree-children tree1))
        (start (phpinspect-tree-start tree1))
        (end (phpinspect-tree-end tree1))
        (value (phpinspect-tree-value tree1)))

    (setf (phpinspect-tree-parent tree1) (phpinspect-tree-parent tree2))
    (setf (phpinspect-tree-children tree1) (phpinspect-tree-children tree2))
    (setf (phpinspect-tree-start tree1) (phpinspect-tree-start tree2))
    (setf (phpinspect-tree-end tree1) (phpinspect-tree-end tree2))
    (setf (phpinspect-tree-value tree1) (phpinspect-tree-value tree2))

    (seq-map (lambda (child)
               (setf (phpinspect-tree-parent child) tree1))
             children)

    (setf (phpinspect-tree-parent tree2) parent)
    (setf (phpinspect-tree-children tree2) children)
    (setf (phpinspect-tree-start tree2) start)
    (setf (phpinspect-tree-end tree2) end)
    (setf (phpinspect-tree-value tree2) value)

    (seq-map (lambda (child)
               (setf (phpinspect-tree-parent child) tree2))
             children)))

(cl-defmethod phpinspect-tree-find-overlapping-children
  ((tree phpinspect-tree) (start integer) (end integer))
  (let* ((region (phpinspect-make-region start end))
         (children (phpinspect-tree-children tree))
         (first-overlapper
          (seq-find (lambda (child) (phpinspect-tree-overlaps child region))
                    children)))
    (when first-overlapper
      (seq-take-while (lambda (child) (phpinspect-tree-overlaps child region))
                      (phpinspect-ll-link children first-overlapper)))))

(defsubst phpinspect-tree-empty-p (tree)
  (and (= 0 (phpinspect-tree-start tree))
       (= 0 (phpinspect-tree-end tree))))

(cl-defmethod phpinspect-tree-find-last-child-before-point ((tree phpinspect-tree) (point integer))
  (catch 'found
    (seq-doseq (child (seq-reverse (seq-into (phpinspect-tree-children tree) 'slice)))
      (when (<= (phpinspect-tree-end child) point)
        (throw 'found child)))))

(cl-defmethod phpinspect-tree-insert-node ((tree phpinspect-tree) (node phpinspect-tree))
  "Insert a new NODE into TREE.

Returns the newly inserted node."
  (cond ((phpinspect-tree-empty-p tree)
         (phpinspect-tree-switch-attributes node tree)

         ;; Return
         tree)
        ((phpinspect-tree-encloses tree node)
         ;; New node is entirely enclosed by tree, check tree's children for
         ;; overlappings.
         (let* ((overlappers (phpinspect-tree-find-overlapping-children
                              tree (phpinspect-tree-start node) (phpinspect-tree-end node)))
                (overlap-count (seq-length overlappers)))
           (if overlappers
               (cond
                ((= 1 overlap-count)
                 (phpinspect-tree-insert-node (seq-elt overlappers 0)
                                              node))
                ((< 1 overlap-count)
                 ;; There are multiple overlapping children. They need to all
                 ;; fit within node, or the hierarchy is broken.
                 (let ((enclosed
                        (seq-take-while
                         (lambda (child) (phpinspect-tree-encloses node child))
                         overlappers))
                       (insert-after-link))
                   (unless (= (seq-length enclosed) overlap-count)
                     (seq-doseq (lap overlappers)
                       (message "overlaps: %s (%d,%d) with %s (%d,%d)"
                                (phpinspect-meta-token (phpinspect-tree-value lap))
                                (phpinspect-tree-start lap)
                                (phpinspect-tree-end lap)
                                (phpinspect-meta-token (phpinspect-tree-value node))
                                (phpinspect-tree-start node)
                                (phpinspect-tree-end node)))
                     (throw 'phpinspect-tree-conflict
                            "Node overlaps multiple children, but does not enclose them all"))

                   (setq insert-after-link (phpinspect-llnode-left
                                            (phpinspect-slice-start enclosed)))
                   (setq enclosed (phpinspect-slice-detach enclosed))
                   (if insert-after-link
                       (phpinspect-ll-insert-right insert-after-link node)
                     ;; If there is nothing to the left of the enclosed regions,
                     ;; we can safely push to the tree's children
                     (phpinspect-ll-push node (phpinspect-tree-children tree)))
                   (setf (phpinspect-tree-parent node) tree)

                   (seq-doseq (child enclosed)
                     (setf (phpinspect-tree-parent child) node))
                   (setf (phpinspect-tree-children node) enclosed))))

             ;; ELSE: No overlap, node can safely be added as child
             (setf (phpinspect-tree-parent node) tree)
             (let* ((right-neighbour (phpinspect-tree-children tree))
                    (right-neighbour-value
                     (seq-find (lambda (child) (< (phpinspect-tree-end child)
                                                  (phpinspect-tree-start node)))
                               right-neighbour)))
               (when right-neighbour-value
                 (setq right-neighbour (phpinspect-ll-link
                                        right-neighbour
                                        right-neighbour-value)))

               (if (phpinspect-llnode-left right-neighbour)
                   (phpinspect-ll-insert-left right-neighbour node)
                 (phpinspect-ll-push node right-neighbour)))))

         ;; Return
         node)
        ((phpinspect-tree-encloses node tree)
         ;; New node encloses entire tree, so it has to become the new root.
         (let* ((parent (phpinspect-tree-parent tree)))
           (if parent
               (progn
                 (phpinspect-ll-relink
                  (phpinspect-ll-link (phpinspect-tree-children parent) tree) node)
                 (setf (phpinspect-tree-parent node) parent)
                 (phpinspect-tree-insert-node node tree)

                 ;; Return
                 node)

             ;; No parent, which means that this is the absolute root node of
             ;; the tree. To keep things consistent, swap all the attributes of
             ;; both trees to keep the reference to the root node intact for the
             ;; caller.
             (progn
               (phpinspect-tree-switch-attributes node tree)
               (phpinspect-tree-insert-node tree node)

               ;; Return tree, as this is the node that value of node has been
               ;; stored in.
               tree))))
        (t (message "parent: %s" (when (phpinspect-tree-value tree)
                                   (phpinspect-meta-token (phpinspect-tree-value tree))))
           (message "perspective child: %s"
                    (when (phpinspect-tree-value node)
                      (phpinspect-meta-token (phpinspect-tree-value node))))

           (throw 'phpinspect-tree-conflict
                  (format "Tree does not enclose or get enclosed. \nTree: (%d,%d,%s) \n\nPerspective child: (%d,%d,%s)"
                          (phpinspect-tree-start tree)
                          (phpinspect-tree-end tree)
                          (if (phpinspect-tree-parent tree) "non-root" "root")
                          (phpinspect-tree-start node)
                          (phpinspect-tree-end node)
                          (if (phpinspect-tree-parent node) "non-root" "root"))))))

(cl-defmethod phpinspect-tree-traverse-overlapping ((tree phpinspect-tree) (point integer))
  "Traverse TREE for intervals overlapping POINT.

Returns list of values from overlapping trees, sorted by interval
width with the smallest interval as car."
  (when (phpinspect-tree-overlaps tree point)
    (let* ((from-end (- (phpinspect-tree-end tree) point))
           (from-start (- point (phpinspect-tree-start tree)))
           (overlapper
            (catch 'found
              (let ((children (seq-into (phpinspect-tree-children tree) 'slice)))
                (when (> from-start from-end)
                  (setq children (seq-reverse children)))

                (phpinspect-doslice (child children)
                  (when (phpinspect-tree-overlaps child point)
                    (throw 'found child)))))))

      (if overlapper
          `(,@(phpinspect-tree-traverse-overlapping overlapper point) ,(phpinspect-tree-value tree))
        `(,(phpinspect-tree-value tree))))))

(cl-defmethod phpinspect-tree-traverse-overlapping ((tree phpinspect-tree) region)
  "Traverse TREE for intervals overlapping POINT.

Returns list of values from overlapping trees, sorted by interval
width with the smallest interval as car."
  (when (phpinspect-tree-overlaps tree region)
    (let* ((overlappers (phpinspect-tree-find-overlapping-children
                         tree
                         (phpinspect-region-start region)
                         (phpinspect-region-end region))))
      (if overlappers
          (let ((all-overlappers))
            (seq-doseq (overlapper overlappers)
              (setq all-overlappers
                    (append all-overlappers (phpinspect-tree-traverse-overlapping overlapper region))))
            `(,@all-overlappers ,(phpinspect-tree-value tree)))
        `(,(phpinspect-tree-value tree))))))

(cl-defmethod phpinspect-tree-shift ((tree phpinspect-tree) (delta integer))
  (phpinspect-tree-traverse (node tree)
    (setf (phpinspect-tree-start node) (+ (phpinspect-tree-start node) delta))
    (setf (phpinspect-tree-end node) (+ (phpinspect-tree-end node) delta))))

(cl-defmethod phpinspect-tree-widen-after-point
  ((tree phpinspect-tree) (point integer) (delta integer) &optional fn)
  "Widens all nodes of TREE that start or end after POINT by DELTA.

When FN is set, it is called once for each widened tree node,
with its value as argument."
  (let ((tree-children (phpinspect-tree-children tree))
        (children))
    (cond
     ((phpinspect-tree-overlaps tree point)
      (setf (phpinspect-tree-end tree) (+ (phpispect-tree-end tree) delta))
      (let* ((first-match
              (seq-find (lambda (child) (or (phpinspect-tree-overlaps child point)
                                            (phpinspect-tree-starts-after child point)))
                        tree-children)))
        (setq children
              (seq-take-while (lambda (child) (or (phpinspect-tree-overlaps child point)
                                                  (phpinspect-tree-starts-after child point)))
                              (phpinspect-ll-link
                               tree-children first-match))))

      (when fn (funcall fn (phpinspect-tree-value tree))))
     ((phpinspect-tree-starts-after tree point)
      (setf (phpinspect-tree-start tree) (+ (phpinspect-tree-start tree) delta))
      (setf (phpinspect-tree-end tree) (+ (phpinspect-tree-end tree) delta))
      (setq children tree-children)

      (when fn (funcall fn (phpinspect-tree-value tree)))))


    (when children
      (seq-doseq (child children)
        (phpinspect-tree-widen-after-point child point)))))

(cl-defmethod phpinspect-tree-find-node-starting-at ((tree phpinspect-tree) (point integer))
  (if (= (phpinspect-tree-start tree) point)
      tree
    (catch 'found
      (seq-doseq (child (phpinspect-tree-children tree))
        (when (phpinspect-tree-overlaps tree point)
          (let ((found? (phpinspect-tree-find-node-starting-at child point)))
            (when found? (throw 'found found?))))))))

(cl-defmethod phpinspect-tree-width ((tree phpinspect-tree))
  (- (phpinspect-tree-start tree) (phpinspect-tree-end tree)))

(cl-defmethod phpinspect-tree-find-smallest-overlapping-set ((tree phpinspect-tree) region)
  "Traverse TREE for smallest set of intervals overlapping REGION,

Returns list of values from the set of overlapping trees that
collectively have the smallest width."
  (when (phpinspect-tree-overlaps tree region)
    (let* ((tree-start (phpinspect-tree-start tree))
           (tree-end (phpinspect-tree-end tree))
           (overlappers (phpinspect-tree-find-overlapping-children
                         tree (phpinspect-region-start region)
                         (phpinspect-region-end region)))
           (overlap-count (seq-length overlappers))
           (overlap-start tree-start)
           (overlap-end tree-end))

      (when overlappers
        (setq overlap-start
              (phpinspect-tree-start
               (phpinspect-llnode-value (phpinspect-slice-start overlappers))))
        (setq overlap-end
              (phpinspect-tree-end
               (phpinspect-llnode-value (phpinspect-slice-end overlappers)))))

      (if (or (> overlap-start tree-start)
              (< overlap-end tree-end))
          (cond
           ((< 1 overlap-count)
            ;; Overlap of children is smaller, but no point recursing if it already
            ;; spans two children. Return overlappers.
            (seq-map #'phpinspect-tree-value overlappers))
           ((= 1 overlap-count)
            ;; Overlap of single child is smaller, recurse.
            (phpinspect-tree-find-smallest-overlapping-set (seq-elt overlappers 0)
                                                           region)))
        ;; Overlap spans the entire tree, so this already is the smallest
        ;; overlapping set (of one).
        `(,(phpinspect-tree-value tree))))))

(cl-defmethod phpinspect-tree-insert
  ((tree phpinspect-tree) (start integer) (end integer) value)
  "Insert a new interval from START to END linked to VALUE into TREE.

Returns the newly created and inserted node."
  (let ((node (phpinspect-make-tree :start start
                                    :end end
                                    :value value)))
    (phpinspect-tree-insert-node tree node)))

(cl-defmethod phpinspect-tree-detach ((tree phpinspect-tree))
  "Detach tree from parent without renewing its value map."
  (let ((parent (phpinspect-tree-parent tree)))
    (when parent
      (let ((parent-link (phpinspect-ll-link (phpinspect-tree-children parent)
                                             tree)))
        (unless parent-link
          (phpinspect--log "No parent link for node, trying to find it manually")
          (message "No parent link for tree of %s" (phpinspect-tree-meta-token tree))
          (message "Parent: %s" (phpinspect-tree-meta-token parent))
          (setq parent-link
                (seq-find (lambda (child) (eq child tree))
                          (phpinspect-tree-children parent))))

        (if parent-link
            (phpinspect-llnode-detach parent-link)
          (phpinspect--log "[WARNING] No parent link for node."))


        (setf (phpinspect-tree-parent tree) nil)))
    tree))

(defmacro phpinspect-tree-traverse (place-and-tree &rest body)
  (declare (indent defun))
  (let ((stack (gensym))
        (child (gensym))
        (children (gensym)))
    `(let ((,stack (list ,(cadr place-and-tree)))
           (,(car place-and-tree)))
       (while (setq ,(car place-and-tree) (pop ,stack))
         ,@body
         (let ((,children (phpinspect-tree-children ,(car place-and-tree))))
           (unless (seq-empty-p ,children)
             (seq-doseq (,child ,children)
               (push ,child ,stack))))))))

(defsubst phpinspect-make-region (start end)
  (list start end))

(defalias 'phpinspect-region-start #'car)
(defalias 'phpinspect-region-end #'cadr)

(defsubst phpinspect-region-size (region)
  (- (phpinspect-region-end region) (phpinspect-region-start region)))

(defsubst phpinspect-region> (reg1 reg2)
  (> (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defsubst phpinspect-region< (reg1 reg2)
  (< (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defsubst phpinspect-region-overlaps-point (reg point)
  (and (> (phpinspect-region-end reg) point)
       (<= (phpinspect-region-start reg) point)))

(defsubst phpinspect-region-overlaps (reg1 reg2)
  (or (phpinspect-region-reg2s-point reg1 (phpinspect-region-start reg2))
      (phpinspect-region-reg2s-point reg1 (- (phpinspect-region-end reg2) 1))
      (phpinspect-region-reg2s-point reg2 (phpinspect-region-start reg1))
      (phpinspect-region-reg2s-point reg2 (- (phpinspect-region-end reg1) 1))))

(defsubst phpinspect-region-encloses (reg1 reg2)
  (and (<= (phpinspect-region-start reg1) (phpinspect-region-start reg2))
       (>= (phpinspect-region-end reg1) (phpinspect-region-end reg2))))

(provide 'phpinspect-tree)
