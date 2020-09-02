(in-package :machine-learning)

(defun linear-regression (weights)
  (let* ((wi (butlast weights))
	 (b (first (last weights)))
	 (args (loop for nil in wi collecting (gensym))))
    (eval `#'(lambda ,args
	       (+ ,@(mapcar #'(lambda (a w)
				`(* ,a ,w))
			    args wi)
		  ,b)))))

(defun linear-between (fn min max)
  (list (list min (funcall fn min))
	(list max (funcall fn max))))

(defun average (points)
  (loop for (x y) in points
     summing x into c-x
     summing y into c-y
     counting t into i
     finally (return (values (/ c-x i) (/ c-y i)))))

(defun r-pearson (points)
  (multiple-value-bind (av-x av-y)
      (average points)
    (loop for (x y) in points
       summing (* (- x av-x) (- y av-y)) into cov-xy
       summing (expt (- x av-x) 2) into var-x
       summing (expt (- y av-y) 2) into var-y
       finally (return (/ cov-xy (sqrt (* var-x var-y)))))))

(defun r-sqrd (points)
  (expt (r-pearson points) 2))

(defun simple-linear-regression (points)
  (multiple-value-bind (av-x av-y)
  (average points)
    (loop for (x y) in points
       summing (* (- x av-x) (- y av-y)) into s
       summing (expt (- x av-x) 2) into s-sqrd
       finally (let ((a (/ s s-sqrd)))
		 (return (list a (- av-y (* a av-x))))))))
