(in-package :machine-learning)

(defun running-single (input weights threshold net-fn activation-fn)
  (funcall activation-fn (funcall net-fn weights input) threshold))

(defun perceptron-update (source target output weights learning-rate)
  (if (eq output target)
      (list weights nil)
      (list (mapcar #'(lambda (weight source)
			(+ weight (* learning-rate target source)))
		    weights source)
	    t)))

(defun perceptron-stop-condition (old update current-p tolerance current-cicles max-cicles)
  (declare (ignorable old tolerance current-cicles max-cicles))
  (or (second update) current-p))

(defun iterative-training (source-list target-list initial-weights threshold learning-rate tolerance max-cicles update-fn stop-fn net-fn activation-fn)
  (let (quadratic-error quadratic-error-aux)
    (labels ((rec (w p src trg cicle)
	       (if (and src trg)
		   (let* ((output (running-single (car src) w threshold net-fn activation-fn))
			  (target (car trg))
			  (update (funcall update-fn (car src) target output w learning-rate)))
		     (push (expt (- target output) 2) quadratic-error-aux)
		     (rec (first update)
			  (funcall stop-fn w update p tolerance cicle max-cicles)
			  (cdr src) (cdr trg) cicle))
		   (progn
		     (push (list cicle
				 (apply #'+ quadratic-error-aux)
				 1)
			   quadratic-error)
		     (setf quadratic-error-aux nil)
		     (if p
			 (rec w nil source-list target-list (1+ cicle))
			 w)))))
      (values (rec initial-weights t source-list target-list 0)
	      (nreverse quadratic-error)))))

(defun scatter-plot (output table boundary)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(:pngcairo) :output output)
    (gp :set :palette '("defined (-1 'red', 1 'blue')"))
    (plot
     (lambda ()
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
  (let ((er (- target output)))
    (list (mapcar #'(lambda (weight source)
		      (+ weight (* learning-rate er source)))
		  weights source)
	  t)))

(defun adaline-activation (net threshold)
  (declare (ignore threshold))
  net)

(defun adaline-stop-condition (old update current-p tolerance current-cicles max-cicles)
  (if (> current-cicles max-cicles)
      nil
      (let ((min 1))
	(mapcar #'(lambda (o-w n-w)
		    (let ((s (- n-w o-w)))
		      (when (< s min)
			(setf min s))))
		old (first update))
	(or (> min tolerance) current-p))))
