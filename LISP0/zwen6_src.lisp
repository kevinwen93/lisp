;; take a target atom and list in;
;;return the number of occurrence of that atom in that list;
(defun count-occur (s lst)
  (cond
    ;; type check of s
    ((not (atom s)) nil)
    ;; type check of lst
    ((not (listp lst)) nil)
    ;;base case if end of recur, return 0
    ((null lst) 0)
    ;;if hit plus one and to next
    ((equal s (car lst)) (+ 1 (count-occur s (cdr lst))))
    ;;if s in a nested loop, flatten it
    ((not (atom (car lst))) (count-occur s (append (car lst) (cdr lst))))
    ;;if not hit pass to next
    (t (count-occur s (cdr lst)))))

;;take two exprssion in which could be atom or list;
;;return true if they are exactly equal;
;;if not exactly equal, return the result that include partial equal;
;;if not equal at all, return nil;
(defun subexpr (expr1 expr2)
  (cond
    ;;if hit 
    ((equal expr1 (car expr2)) expr2)
    ;;end of the recur
    ((null expr2) nil)
    ;;special case for list in list
    ((and
      (not(equal (type-of expr1) (type-of (cdr expr2))))
      (not (atom (cadr expr2))))
     (subexpr expr1 (cadr expr2)))
    ;;base case if equal
    ((and
     (equal (type-of expr1) (type-of expr2))
     (equal expr1 expr2)) t)
    ;;base case pass to check next one
    (t (subexpr expr1 (cdr expr2)))))

;;take a list in, which could be nested list;
;;return a not nested list
(defun my-flatten (lst)
  (cond
    ;;if it is atom, make it list
    ((atom lst) (list lst))
    ;;if the list reach the end, return nil
    ((null lst) nil)
    ;;call recursively my-flatten for each item in the list;
    ;;then append them together;
    (t (reduce #'append (mapcar #'my-flatten lst)))))

;;take two list in;
;;return a list contain the intersection of them
(defun my-intersection(l1 l2)
  ;;initial an empty set
  (setq result ())
  (block nil
    (cond
      ;; null list check
      ((null l1) or (null l2) nil)
      ;; check each member in first list if they are in the second one
      (t (loop for data in l1 do
	    (push (car (member data l2 :test #'tree-equal)) result))))
  (return result)))
    
;;take a number n in
;;return the corresponding fib number
(defun fib(n)
  ;;type check if input is a interger
  (block nil
    (cond
      ((not (integerp n)) (return nil))
      ;;if it is 0 return 0
      ((equal n 0) (return 0))
      ;;go to the fib loop
      (t (loop repeat n
	    for f0 = 0 then f1
	    and for f1 = 1 then (+ f1 f0)
	    finally (return f1))))))

;;take two list contained pairs of data in
;;return a list also contained pairs of data with
;;first element in the pair added up if the seond element it is the same
(defun merge-occurrence-counts(lst1 lst2)
  ;;intial a return list
  (setq result ())
  (block nil
    ;;nested loop for cheking if each element in lst2 is in lst1
    ;;if yes, put in the return list
    (loop for setout in lst2 do
	 (progn
	   (setq c 0)
	   (loop for setin in lst1 do
		(if (equal (cadr setout) (cadr setin))
		    (progn
		      (push (append (list(+ (car setin)
					    (car setout))) (cdr setout)) result)
		      ;;use a flag to check if this element has been put in list
		      (setq c 1)))))
       ;;if the element in lst1 can't find similar in lst2;
       ;;put it in the return list
	 (if (equal c 0)
	     (push setout result)))
    ;;do it agian for each element in lst1 if it is in result list
    (loop for setout in lst1 do
	 (progn
	   ;;also use flag
	 (setq c 0)
	 (loop for setin in result do
	      (if (equal (cadr setout) (cadr setin))
		  (setq c 1)))
	 ;;if can't find it in the resut list put it in the result list
	 (if (equal c 0)
	     ( push setout result))))
  (return result)))

		
		
       
  
