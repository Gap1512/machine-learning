(in-package :machine-learning)

(defun neuron-output (inputs neuron-list fn)
  (let ((net (reduce #'+ (mapcar #'* inputs neuron-list))))
    (values (funcall fn net)
	    net)))

(defun layer-output (inputs layer-list fn)
  (mapcar #'(lambda (neuron)
	      (neuron-output inputs neuron fn))
	  layer-list))

(defun mlnn-output (inputs mlnn-list fn)
  (do ((result inputs (layer-output (append result (list 1)) (car layer-list) fn))
       (layer-list mlnn-list (cdr layer-list)))
      ((not layer-list) (car result))))

(defun bipolar-sigmoid (x)
  (- (/ 2 (+ 1 (exp (- x)))) 1))

(defun bipolar-sigmoid^1 (x)
  (let ((f (bipolar-sigmoid x)))
    (* 1/2 (1+ f) (- 1 f))))

(defun small-delta-k (target net output fn^1)
  (* (- target output) (funcall fn^1 net)))

(defun small-delta-j (net delta-list weight-list fn^1)
  (* (reduce #'+ (mapcar #'* delta-list weight-list)) (funcall fn^1 net)))

(defun new-weight (learning-rate small-delta input)
  (* learning-rate small-delta input))

(defun new-weight-list (old-weight-list inputs learning-rate small-delta)
  (mapcar #'(lambda (old-weight input)
	      (+ old-weight (new-weight learning-rate small-delta input)))
	  old-weight-list inputs))

(defun new-layer-list (old-layer-list small-delta-list inputs learning-rate)
  (mapcar #'(lambda (old-weight-list small-delta)
	      (new-weight-list old-weight-list inputs learning-rate small-delta))
	  old-layer-list small-delta-list))

(defun new-mlnn-list (old-mlnn-list source target fn fn^1 learning-rate)
  (labels ((loop-delta (old-layer-list next-layer-list inputs delta-list)
	      (do ((i 0 (1+ i))
		   (neuron old-layer-list (cdr neuron))
		   result)
		  ((not neuron) (nreverse result))
		(let ((weights (mapcar #'(lambda (y) (nth i y))
				       next-layer-list)))
		  (push (small-delta-j (neuron-output inputs (car neuron) fn)
				       delta-list weights fn^1)
			result))))
	   (rec (layers inputs)
	     (let ((x (cdr layers)))
	       (if x
		   (let ((old-layer-list (car layers)))
		     (multiple-value-bind (next-layers delta quadratic-error)
			 (rec x (append (layer-output inputs old-layer-list fn) (list 1)))
		       (let ((delta-j-list (loop-delta old-layer-list
						       (car next-layers)
					               inputs
					               delta)))
			 (values (cons (new-layer-list old-layer-list
						       delta-j-list
						       inputs
						       learning-rate)
				       next-layers)
				 delta-j-list quadratic-error))))
		   (let* ((quadratic-error 0)
			  (last-layer (car layers))
			  (delta-k-list
			   (mapcar #'(lambda (neuron)
				       (multiple-value-bind (output net)
					   (neuron-output inputs neuron fn)
					 (setf quadratic-error
					       (+ quadratic-error
						  (expt (- output target)
							2)))
					 (small-delta-k target net output fn^1)))
				   last-layer)))
		     (values (list (new-layer-list last-layer
						   delta-k-list
						   inputs
						   learning-rate))
			     delta-k-list
			     (/ quadratic-error 2)))))))
    (rec old-mlnn-list (append source (list 1)))))

(defun multiple-source-new-mlnn-list (initial-mlnn-list source-list target-list fn fn^1 learning-rate)
  (do ((mlnn-list initial-mlnn-list)
       delta
       (err 0)
       (source source-list (cdr source))
       (target target-list (cdr target)))
      ((or (not source) (not target)) (values mlnn-list err))
    (setf (values mlnn-list delta err)
	  (new-mlnn-list mlnn-list (car source) (car target) fn fn^1 learning-rate))))

(defun iterative-retropropagation (initial-mlnn-list source-list target-list fn fn^1 learning-rate cycles tolerance)
  (do ((i 0 (1+ i))
       (mlnn-list initial-mlnn-list)
       (err 0)
       err-list)
      ((or (> i cycles)
	   (and (< err tolerance)
		(> i 0)))
       (values mlnn-list
	       (nreverse err-list)))
    (setf (values mlnn-list err)
	  (multiple-source-new-mlnn-list mlnn-list source-list target-list fn fn^1 learning-rate))
    (push (list i err 1) err-list)))

(defun random-layer-list (min max n-neurons n-weights)
  (loop repeat n-neurons
       collecting (random-weights n-weights min max)))

(defun random-mlnn-list (min max n-inputs &rest configs)
  (do ((layers configs (cdr layers))
       (last-n n-inputs (car layers))
       result)
      ((not layers) (nreverse result))
    (push (random-layer-list min max (car layers) (1+ last-n))
	  result)))
