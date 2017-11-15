(defun show-failed-result (function-name input expected result)
  (let ((*print-pretty* nil))  ;; a global variable that inserts junk when true
    (format t "Function \"~S\" failed on inputs \"~{~S~^ ~}\"; expected \"~S\"; got \"~S\".~%"
	    function-name input expected result)))

(defun set-equal (x1 x2)
  (null (set-exclusive-or x1 x2 :test #'equal)))

(defun set-and-length-equal (x1 x2)
  (and (listp x1) (listp x2)
	   (eq (length x1) (length x2))
       (set-equal x1 x2)))

(defun test (function-name &optional (testfn #'equal) (invalid-input nil))
  (let ((trials (if invalid-input
                  (get-invalid-input-tests function-name) 
                  (get-tests function-name)))
	(passed 0))
    (dolist (trial trials)
      (let* ((input (first trial))
	     (expected (second trial))
	     (result (handler-case (apply function-name (copy-tree input)) (error (e) "ERROR!!!"))))
	(if (funcall testfn result expected)
          ;(equal result expected)
	    (incf passed)
	    (show-failed-result function-name input expected result))))
    (format nil "~s / ~s tests passed for function ~s."
	    passed (length trials) function-name)))

(defparameter *tests*
  (make-hash-table)
  "A map from function symbols to lists of valid input test cases.")

(defparameter *invalid-input-tests*
  (make-hash-table)
  "A map from function symbols to lists of invalid input test cases.")


(defun create-invalid-input-test (function-name inputs expected)
  "Inserts a new invalid input test case into the global list for function name"
  (setf (gethash function-name *invalid-input-tests*)
	(append (get-invalid-input-tests function-name)
		(list (list inputs expected)))))

(defun create-test (function-name inputs expected)
  "Inserts a new test case into the global list for function-name."
  (setf (gethash function-name *tests*)
	(append (get-tests function-name)
		(list (list inputs expected)))))

(defun create-test-1 (function-name input expected)
  "Inserts a new test case into the global list for function-name."
  (setf (gethash function-name *tests*)
	       (append (get-tests function-name)
		       (list (list (list input) expected)))))

(defun create-test-2 (function-name input1 input2 expected)
  "Inserts a new test case into the global list for function-name."
  (setf (gethash function-name *tests*)
	(append (get-tests function-name)
		(list (list (list input1 input2) expected)))))

(defun get-tests (function-name)
  (gethash function-name *tests*))

(defun get-invalid-input-tests (function-name)
  (gethash function-name *invalid-input-tests*))
