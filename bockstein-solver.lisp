;;;; bockstein-solver.lisp
;;;;
;;;; Author: Eric Peterson and Robert Smith
;;;;
;;;; This file contains a skeleton of an iterative routine that builds a description of the Bockstein spectral sequence
;;;; computing the cohomology of the minimal 1-dimensional quotient of the Lubin-Tate representation of the Morava
;;;; stabilizer group.  Some of the mathematics portrayed here is standard; some is conjectural.

(defun ideal-degree (term)
  "Calculates the value k for which term / m(aximum-ideal)^k is zero but term / (maximum-ideal)^(k+1) is nonzero."
  ...)

(defun eliminate-ideal-prefactors (term)
  "Produces the maximal factor of a term which does not intersect the maximum-ideal."
  ...)

(defun advance-action-formulas (truncation-degree)
  "Calculates a new table of action formulas which are more accurate than those given by TRUNCATION-DEGREE."
  ;; this seems tricky. do we increment by 1? do we jump up to the next p-adic valuation?
  ;; how do we know what's computationally inexpensive? how do we know what's required?
  ...
  (values action-formulas new-truncation-degree))

(defstruct processing-term
  (base-term
   correction-factors
   character-degree
   cohomological-degree))

(defstruct differential-output
  ...)

(defun find-shorter-differential (target bound differential-table)
  "Attempts to find "
  )

(defstruct cocycle-output
  ...)

(defstruct coboundary-output
  ...)

(defun initialize-processing-queue ()
  ;; some kind of priority queue: low cohomological degrees first, then low character degrees within that
  ...)

(defun select-next-term (processing-queue)
  ...)

(defun apply-coboundary-formula (term action-formulas)
  "Given term: G^n --> V and action-formulas: G x V --> V, compute (del term): G^(n+1) --> V.

Guaranteed to return only the bottom-degree nonzero information."
  ...)

(defun main-loop (&key
		    (truncation-degree 0)
		    (processing-queue (initialize-processing-queue))
		    (differential-output (make-differential-output))
		    (cocycle-output (make-cocycle-output))
		    (coboundary-output (make-cobounary-output)))
  "Main loop for computing the cocycle/coboundary table for the Lubin-Tate representation of the stabilizer group."
  (multiple-value-bind (action-formulas truncation-degree)
      (advance-action-formulas truncation-degree)
    (do					; inner loop
     (let* ((next-term (select-next-term processing-queue))
	    (differential (apply-coboundary-formula next-term action-formulas))
	    (page (- (ideal-degree differential) (ideal-degree next-term))))
       ;; if we got an exact term, try again with an enlarged truncation degree.
       ;; TODO: check that infinite divisibility either doesn't happen or happens rarely and predictably in the known cases.
       (cond
	 ((null-term-p differential)
	  (return-from main-loop
	    (main-loop :truncation-degree truncation-degree
		       :processing-queue processing-queue
		       :differential-output differential-output
		       :cocycle-output cocycle-output
		       :coboundary-output coboundary-output))))
       ;; if we've seen this term before, use that to add a correction term
       ((find-shorter-differential differential page differential-output)
	(append (term-subtract 1 (find-shorter-differential differential page differential-output))
      	        (processing-term-correction-factors next-term)))
       ;; otherwise, this particular coboundary is fresh.
       (t
	(let ((bare-cocycle (eliminate-ideal-prefactors (differential-target differential))))
	  (store-into-differential-output differential differential-output)
	  (store-into-cocycle-output bare-cocycle cocycle-output)
	  (store-into-processing-queue bare-cocycle)
	  (store-into-coboundary-output (differential-target differential) coboundary-output)
	  (dequeue-top-term processing-queue)
	  ;; TODO: eliminate potential cocycles from the processing-queue based on coboundary calculation
	  ;; TODO: figure out where replacing a term with its pth iterate goes.
	  ;;       (does this happen in place of dequeue-top-term?)
	  ))))))
