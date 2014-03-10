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
    (when items 
      (let ((item (pop items)))
        (if items 
          (progn (incf (cadar items) (car item))
                 (cddr item))
          (cddr item))))))

(defgeneric queue-add (queue new ticks))
(defmethod queue-add ((queue tick-queue) new (ticks number))
  (with-slots (items) queue
    (if items 
      (labels ((walk (l &optional (accum 0))
                 (incf accum (cadar l))
                 (cond ((<= ticks (- (caar l) accum))
                        (push (car l) (cdr l))
                        (setf (car l) `(,ticks ,(- accum) . ,new)))
                       ((cdr l) (walk (cdr l) accum))
                       (t (setf (cdr l) `((,ticks ,(- accum) . ,new)))))))
        (walk items))
      (push `(,ticks 0 . ,new) items))))

(defgeneric queue-data (queue))
(defmethod queue-data ((queue tick-queue))
  (slot-value queue 'items))
