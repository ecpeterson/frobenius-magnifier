;;;; padic-sums.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:frobenius-magnifier)


(deftype adic-sum ()
  "A finite sum of the form sum_j a_j p^j, with |a_j| << p."
  '(vector integer))

(defun make-adic-sum-by-size (size)
  (make-array size :element-type 'integer))

(defun make-adic-sum (&rest entries)
  (make-array (length entries) :element-type 'integer :initial-contents entries))

(defmethod + ((x adic-sum) (y adic-sum))
  (let ((res (make-adic-sum-by-size (max (length x) (length y)))))
    (dotimes (index (length res))
      (let ((xx (if (< index (length x)) (aref x index) 0))
	    (yy (if (< index (length y)) (aref y index) 0)))
	(setf (aref res idx)
	      (+ xx yy))))))

(defmethod * ((x adic-sum) (y adic-sum))
  (let ((res (make-adic-sum-by-size (+ (length x) (length y) -1))))
    (dotimes (j (length res))
      (dotimes (i j)
	(let ((xx (if (< i (length x)) (aref x i) 0))
	      (yy (if (< (- j i) (length y)) (aref y (- j i)) 0)))
	  (incf (aref res j) (* xx yy)))))))

(defmethod * ((x adic-sum) (y integer))
  (let ((res (make-adic-sum-by-size (length x))))
    (dotimes (j (length x))
      (setf (aref res j) (* (aref x j) y)))))

(defmethod * ((x integer) (y adic-sum))
  (* y x))

(defmethod = ((x adic-sum) (y adic-sum))
  (dotimes (j (max (length x) (length y)) t)
    (cond
      ((and (< j (length x))
	    (< j (length y)))
       (unless (= (aref x j) (aref y j))
	 (return-from = nil)))
      ((< j (length x))
       (unless (= (aref x j) 0)
	 (return-from = nil)))
      ((< j (length y))
       (unless (= (aref y j) 0)
	 (return-from = nil)))
      (t
       (return-from = t)))))

(defmethod <= ((x adic-sum) (y adic-sum))
  (dotimes (j (max (length x) (length y)) t)
    (cond
      ((and (< j (length x))
	    (= (aref y j) 0))
       t)				; continue
      ((< j (length x))
       (<= 0 (aref y j)))
      ((and (< j (length y))
	    (= (aref x j) 0))
       t)				; continue
      ((< j (length y))
       (<= (aref x j) 0))
      ((= (aref x j) (aref y j))
       t)				; continue
      ((> (aref x j) (aref y j))
       (return-from <= nil))
      (t
       (return-from <= t)))))

(defmethod expt ((x adic-sum) (y integer))
  ;; NOTE: this acts a lil differently for x-adic sums versus p-adic sums,
  ;;       where we sometimes care about the pth binomial coefficient
  ;;       contributing an extra p. this feels like we want to be extremely
  ;;       flexible about the different places where we'd allow an adic-sum
  ;;       to appear: in an array index, even...
  (error "Not yet implemented."))
