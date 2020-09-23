(in-package :machine-learning)

(defun initial-centers (k points)
  (let ((lt (length points)))
    (loop repeat k collecting
	 (nth (random lt) points))))

(defun euclidean-distance (point-1 point-2)
  (sqrt (reduce #'+ (mapcar #'(lambda (p q)
				(expt (- p q) 2))
			    point-1 point-2))))

(defun group-point (point centers)
  (labels ((rec (lst i result)
	     (if lst
		 (rec (cdr lst)
		      (1+ i)
		      (let ((dist (euclidean-distance
				   point
				   (car lst))))
			(if (< dist (first result))
			    (list dist i)
			    result)))
		 result)))
    (rec (cdr centers) 1
	 (list (euclidean-distance point (car centers)) 0))))

(defun update-centers (k points-pos)
  (loop for i from 0 upto (1- k)
     collect (let ((lst (remove-if-not
			 #'(lambda (p)
			     (eq (first (last p)) i))
			 points-pos)))
	       (if lst
		   (butlast (multiple-value-list
			     (average lst)))
		   '(0 0)))))

(defun k-means (k points initial-centers max-iterations tolerance)
  (do* ((distances nil (mapcar #'(lambda (p)
				   (group-point p centers))
			       points))
	(old-err nil err)
	(err nil (reduce #'+ distances :key #'first))
	(b nil (mapcar #'second distances))
	(errs nil (cons (list i err) errs))
	(points-pos nil (mapcar #'(lambda (p b)
				    (append p (list b)))
				points b))
	(centers initial-centers (update-centers k points-pos))
	(i 0 (1+ i)))
       ((or (> i max-iterations)
	    (and (> i 1)
		 (<= (- old-err err) tolerance)))
	(values centers points-pos errs err))))

(defun search-proc (c-point rnd)
  (< (first c-point) rnd (second c-point)))

(defun d-point (point distance)
  (cons distance point))

(defun c-points (d-points)
  (do ((sum (reduce #'+ d-points :key #'first))
       (points d-points (cdr points))
       (aux 0 acc)
       (acc 0)
       (res nil (append res
			(list (cons aux
				    (cons acc
					  (cdr (car points))))))))
      ((not points) res)
    (setf acc (+ acc (/ (first (car points)) sum)))))

(defun roulette (c-points)
  (let ((rnd (random 1.0)))
    (nthcdr 2 (find-if #'(lambda (x)
			   (search-proc x rnd))
		       c-points))))

(defun k-means++ (k points)
  (do ((i 1 (1+ i))
       (centers (initial-centers 1 points)
		(cons (roulette
		       (c-points
			(mapcar #'d-point
				points
				(lesser-distance points centers))))
		      centers)))
      ((>= i k) centers)))

(defun lesser-distance (points centers)
  (mapcar #'first (mapcar #'(lambda (p)
			      (group-point p centers))
			  points)))

(defun clustering (points generation-fn max-iterations-per-cycle
		   tolerance-per-cycle max-iterations tolerance)
  (do ((i 1 (1+ i))
       (errs-k nil (cons (list i err) errs-k))
       centers points-pos errs err)
      ((or (>= i max-iterations)
	   (and (> i 2)
		(< (- (cadadr errs-k) (cadar errs-k))
		   tolerance)))
       (values centers points-pos errs err (nreverse errs-k) i))
    (setf (values centers points-pos errs err)
	  (k-means i points (funcall generation-fn i points)
		   max-iterations-per-cycle tolerance-per-cycle))))
