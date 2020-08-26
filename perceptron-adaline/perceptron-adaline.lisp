(in-package :machine-learning)

(defun running-single (input weights threshold net-fn activation-fn)
  (funcall activation-fn (funcall net-fn  weights input) threshold))

(defun perceptron-update (source target output weights learning-rate)
  (if (eq output target)
      (list weights nil)
      (list (mapcar #'(lambda (weight source)
			(+ weight (* learning-rate target source)))
		    weights source)
	    t)))

(defun perceptron-stop-condition (old update current-p)
  (declare (ignorable old))
  (or (second update) current-p))

(defun iterative-training (source-list target-list initial-weights threshold learning-rate
			   update-fn net-fn activation-fn)
  (labels ((rec (w p src trg)
	     (if (and src trg)
		 (let ((update (funcall update-fn
				(car src)
				(car trg)
				(running-single (car src) w threshold net-fn activation-fn)
				w
				learning-rate)))
		   (rec (first update) (perceptron-stop-condition w update p) (cdr src) (cdr trg)))
		 (if p (rec w nil source-list target-list) w))))
    (rec initial-weights t source-list target-list)))

(defun scatter-plot (output table boundary)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(:pngcairo) :output output)
    (gp :set :palette '("defined (-1 'red', 1 'blue')"))

    (plot (lambda ()
	    (loop
	       for p in boundary
	       do (format t "~&~{~a~^ ~}" p)))
	  :title "Boundary"
	  :with '(:lines))

    (plot
     (lambda ()
       (loop
	  for p in table
	  do (format t "~&~{~a~^ ~}" p)))
     :title "Points"
     :with '(:points :pt 7 :lc :palette)))
  output)

(defun linear-boundary (weights threshold min max)
  (destructuring-bind (w1 w2 b) weights
    (labels ((equation (x) (/ (- threshold b (* x w1)) w2)))
      (list (list min (equation min))
	    (list max (equation max))))))

(defun random-weights (n min max)
  (let ((range (float (- max min))))
    (loop for i from 1 upto n collecting (+ min (random range)))))

(defun adaline-update (source target output weights learning-rate)
  (list (mapcar #'(lambda (weight source)
		    (+ weight (* learning-rate (- target output) source)))
		weights source)
	t)))
