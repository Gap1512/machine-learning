(in-package :machine-learning)

(defun rosenbrock (lst)
  (labels ((rec (lst nxt res)
		  (if nxt
		      (let ((x (car lst)))
			(rec (cdr lst)
			     (cadr lst)
			     (+ res
				(+ (expt (- 1 x) 2)
				   (* 100 (expt (- nxt (* x x)) 2))))))
		      res)))
    (rec lst (cadr lst) 0)))

(defun plot-3d-from-top (output fn min max step &optional (color-min "blue") (color-max "red"))
  (scatter-plot output (do ((x min (+ step x))
			    res)
			   ((> x max) res)
			 (do ((y min (+ step y)))
			     ((> y max))
			   (let ((point (list x y)))
			     (push (append point (list (funcall fn point)))
				   res))))
		nil 0 0 color-min color-max))

(defun initial-population (size n-variables min max)
  (loop repeat size
       collecting (random-weights n-variables min max)))

(defun mutation (f x-1 x-2 x-3)
  (mapcar #'(lambda (a b c)
	      (+ a (* f (- c b))))
	  x-1 x-2 x-3))

(defun crossover (target donor c-rate min max)
  (labels ((choose (x v r i index)
	     (let ((c (if (and (> r c-rate) (not (eq i index)))
			  x v)))
	       (cond
		 ((< c min) min)
		 ((> c max) max)
		 (t c)))))
    (do* ((i (length target) (1- i))
	  (x target (cdr x))
	  (v donor (cdr v))
	  (index (random i))
	  res)
	 ((or (not x) (not v))
	  (nreverse res))
      (push (choose (car x) (car v) (random 1.0) i index)
	    res))))

(defun trial (target trial fitness-fn comparison-fn)
  (if (funcall comparison-fn (funcall fitness-fn trial)
	       (funcall fitness-fn target))
      trial target))

(defun differential-evolution (fn n-variables comparison-fn population-size
			       n-generations c-rate f min max)
  (do ((i 0 (1+ i))
       (population (initial-population population-size n-variables min max)
		   (mapcar #'(lambda (vec)
			       (trial vec
				      (crossover vec
						 (apply #'mutation f
							(loop repeat 3
							   collect (nth (random population-size)
									population)))
						 c-rate min max)
				      fn comparison-fn))
			   population))
       result)
      ((>= i n-generations) (values (best population fn comparison-fn) result))
    (push population result)))

(defun best (lst fn comparison-fn)
  (let ((res (list (car lst) (funcall fn (car lst)))))
    (dolist (item (cdr lst) res)
      (let ((fit (funcall fn item)))
	(when (funcall comparison-fn fit (second res))
	  (setf res (list item fit)))))))
