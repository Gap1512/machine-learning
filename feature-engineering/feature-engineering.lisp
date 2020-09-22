(in-package :machine-learning)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defun parse-double (string)
  (declare (optimize (speed 3)))
  (let ((*read-eval* nil))
    (with-input-from-string (str string)
      (read str nil nil))))

(defun read-csv (filename &key (separator '(#\,)) (key #'parse-double))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
	 while line
	 collect (mapcar key (uiop::split-string line :separator separator)))))

(defun shuffle (list)
  (do ((result (copy-seq list) (swap result i j))
       (j 0 (random i))
       (i (1- (length list)) (1- i)))
      ((zerop i) result)))

(defun swap (list i j)
  (rotatef (nth i list) (nth j list))
  list)

(defun percentual-split (list &rest percentages)
  (if (not (= (reduce #'+ percentages) 1))
      (error "Percentages must add to 1")
      (let ((lt (length list)))
	(labels ((rec (lst rates result)
		   (let ((rest (cdr rates)))
		     (if rest
			 (let ((size (floor (* lt (car rates)))))
			   (rec (nthcdr size lst)
				rest
				(cons (subseq lst 0 size) result)))
			 (values-list (nreverse (cons lst
						      result)))))))
	  (rec list percentages nil)))))

(defun split-at-last (list)
  (list (butlast list)
	(last list)))

(defun rock-mine (item)
  (if (string= item "R")
      0
      1))

(defun multiple-split-at-last (list)
  (loop for i in list
     for (inputs output) = (split-at-last i)
     collecting inputs into source
     collecting (rock-mine (car output)) into target
     finally (return (values source target))))

(defvar *training-set*)
(defvar *validation-set*)
(defvar *test-set*)
(defvar *data* (shuffle (read-csv #p"c:/home/ufu/amaq/feature-engineering/data/sonar.csv")))

(multiple-value-setq (*training-set* *validation-set* *test-set*)
  (percentual-split *data* 0.7 0.15 0.15))

(defun binary-sigmoid (x)
  (/ (1+ (exp (- x)))))

(defun binary-sigmoid^1 (x)
  (let ((f (binary-sigmoid x)))
    (* f (- 1 f))))

(defun mlnn-train-validation (training-set validation-set fn fn^1
			      output-fn threshold learning-rate
			      training-cycles training-tolerance
			      validation-cycles validation-tolerance
			      min max &rest configs)
  (multiple-value-bind (validation-inputs validation-outputs)
      (multiple-split-at-last validation-set)
    (multiple-value-bind (training-inputs training-outputs)
	(multiple-split-at-last training-set)
      (do ((hit-rate-validation (- 1 validation-tolerance))
	   (best-hit-rate 0)
	   (best-err 0)
	   (best-weights nil)
	   (i 0 (1+ i)))
	  ((or (>= i validation-cycles)
	       (> best-hit-rate hit-rate-validation))
	   (values best-weights best-err best-hit-rate))
	(multiple-value-bind (weights err)
	    (iterative-retropropagation (apply #'random-mlnn-list min
					       max (length (car
							    training-inputs))
					       configs)
					training-inputs
					training-outputs fn
					fn^1 learning-rate
					training-cycles
					training-tolerance)
	  (let ((hit-rate (mlnn-hit-rate weights validation-inputs
					 validation-outputs fn output-fn threshold)))
	    (format t "Training #~a Hit Rate: ~a~%" i hit-rate)
	    (when (> hit-rate best-hit-rate)
	      (setf best-weights weights
		    best-err err
		    best-hit-rate hit-rate))))))))

(defun binary-activation (net threshold)
  (if (>= net threshold) 1 0))

(defun mlnn-hit-rate (mlnn-list inputs outputs activation-fn output-fn threshold)
  (let ((hits (mapcar #'(lambda (in out)
			  (if (= (funcall output-fn
					  (mlnn-output in mlnn-list activation-fn)
					  threshold)
				 out)
			      1 0))
	  inputs outputs)))
  (/ (reduce #'+ hits) (length hits))))

(defun mlnn-train-validation-test (training-set validation-set
				   test-set fn fn^1 output-fn
				   threshold learning-rate
				   training-cycles training-tolerance
				   validation-cycles
				   validation-tolerance min max &rest
								  configs)
  (multiple-value-bind (weights err hit-rate)
      (apply #'mlnn-train-validation training-set validation-set fn
	     fn^1 output-fn threshold learning-rate training-cycles
	     training-tolerance validation-cycles validation-tolerance
	     min max configs)
    (multiple-value-bind (test-inputs test-outputs)
	(multiple-split-at-last test-set)
      (let ((test-hit-rate (mlnn-hit-rate weights test-inputs test-outputs fn output-fn threshold)))
	(format t "Test Hit Rate: ~a~%" test-hit-rate)
	(values weights err hit-rate test-hit-rate)))))

(defun save-weights (filename weights)
  (with-open-file (stream filename :direction :output :if-exists
			  :supersede :if-does-not-exist :create)
    (write weights :stream stream)))

(defun load-weights (filename)
  (with-open-file (stream filename)
    (read stream)))
