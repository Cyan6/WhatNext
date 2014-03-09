(defpackage :whatnext
  (:use :common-lisp)
  (:nicknames :wn)
  (:export :tick-queue
           :queue-reset
           :queue-pop
           :queue-add
           :queue-data))

(in-package :whatnext)

(defclass tick-queue ()
  ((items :initform nil
          :initarg :items)))

(defgeneric queue-reset (queue))
(defmethod queue-reset ((queue tick-queue))
  (setf (slot-value queue 'items) nil))

(defgeneric queue-pop (queue))
(defmethod queue-pop ((queue tick-queue))
  (with-slots (items) queue
    (when items (let ((item (pop items)))
                  (if items 
                    (progn (incf (cadar items) (car item))
                           (cddr item))
                    (cddr item))))))

(defgeneric queue-add (queue new ticks))
(defmethod queue-add ((queue tick-queue) new (ticks number))
  (with-slots (items) queue
    (if items
      (labels ((walk (l)
                 (setf (caar l) (- (caar l) (cadar l)))
                 (setf (cadar l) 0)
                 (cond ((< ticks (caar l))
                        (setf (cdr l) (cons (car l) (cdr l)))
                        (setf (car l) `(,ticks 0 . ,new)))
                       ((cdr l) (walk (cdr l)))
                       (t (setf (cdr l) `((,ticks 0 . ,new)))))))
        (walk items))
      (push `(,ticks 0 . ,new) items))
    items))

(defgeneric queue-data (queue))
(defmethod queue-data ((queue tick-queue))
  (with-slots (items) queue
    items))
