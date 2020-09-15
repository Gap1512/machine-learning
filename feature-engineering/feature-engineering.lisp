(in-package :machine-learning)

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
			 (values-list (nreverse (cons lst result)))))))
	  (rec list percentages nil)))))

(defun split-at-last (list)
  (values (butlast list)
	(last list)))
