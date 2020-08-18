(defpackage "hebb"
  (:use :cl))

(in-package "hebb")

(defun hebb (source target weights)
  (mapcar #'(lambda (w x)
	      (+ w (* x target)))
	  weights source))

(defun training (source target weights)
  (do* ((w weights (hebb (car src) (car trg) w))
	(src source (cdr src))
	(trg target (cdr trg)))
       ((or (not src) (not trg)) w)))

(defun activation (net threshold)
  (if (>= net threshold) 1 -1))

(defun net (weights input)
  (apply #'+ (mapcar #'* weights input)))

(defun running (inputs weights threshold 
		net-fn activation-fn)
  (mapcar #'(lambda (i)
	      (funcall activation-fn
		       (funcall net-fn 
				weights i)
		       threshold))
	  inputs))

(defun neural-network (training-fn source
		       target initial-weights
		       running-fn inputs
		       threshold net-fn
		       activation-fn)
  (let ((w (funcall training-fn source
		    target initial-weights)))
    (values (funcall running-fn inputs w
		     threshold net-fn
		     activation-fn)
	    w)))

(defun neural-network-comparison
    (training-fn source target initial-weights
     running-fn inputs threshold
     net-fn activation-fn)
  (multiple-value-bind (output weights)
      (neural-network training-fn source
		      target initial-weights
		      running-fn inputs threshold
		      net-fn activation-fn)
    (with-output-to-string (str)
      (format str
	      "Obtained Weights: [~{~a~^ ~}]~%"
	      weights)
      (mapcar #'(lambda (tar out)
		  (format str
"Expected: ~a | Obtained: ~a | [~:[Fail~;Pass~]]~%"
			  tar out (eq tar out)))
	      target
	      output)
      str)))
