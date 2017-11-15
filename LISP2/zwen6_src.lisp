;;This is a helper function.
;;It takes a atom as the only argument.
;;and return true or nil according to if it is a variable.
(defun var-chk (s)
  (cond
    ;;if variable
    ((not (equal (write-to-string s)
		 (string-left-trim "?" (write-to-string s)))) t)
    ;;constant
    (t nil)))

;;This is helper function.
;;It takes a list as the only argument.
;;It returns true if it is an unified list, otherwise returns nil.
(defun check-unifier(ulst)
  (block nil
    (if (or (not (listp ulst));; if list
	    (notevery #'listp ulst));; if every item inside is list
	(return nil)
	(let* ((varlst (mapcar #'car ulst))
	       (conlst (mapcar #'cadr ulst)))
	  (cond
	    ;;if every list inside length 2
	    ((notevery #'null (subst nil '2 (mapcar #'list-length ulst))) nil)
	    ;;first one must be variable
	    ((some #'equal
		   (mapcar #'write-to-string varlst)
		   (mapcar #'(lambda (x) (string-left-trim "?" x))
			   (mapcar #'write-to-string varlst)))nil)
	    ;;second one must be constant
	    ((notevery #'equal
		       (mapcar #'write-to-string conlst)
		       (mapcar #'(lambda (x) (string-left-trim "?" x))
			       (mapcar #'write-to-string conlst)))nil)
	    ;;otherwise true
	    (t t))))))

;;This is a helper function.
;;It takes one list of unified list in as the only argument.
;;It will reutrn nil if arg type wrong.
;;And otherwise, it returns a new list of unified list without which unified list
;;binding two variable to the same constant.
(defun prune-unifiers (ulst-lst)
  (cond
    ((not (listp ulst-lst))nil)
    ((some #'null (mapcar #'check-unifier ulst-lst)) nil)
    (t(labels
	  ((collect-ulst (ulst)
	     (let* ((conlst (mapcar #' cadr ulst)))
	       (cond ((notevery #'null
				(mapcar #' (lambda (x)
					     (member x (cdr (member x conlst)))) conlst))
		      nil)
		     (t (list ulst))))))
	(mapcan #' collect-ulst ulst-lst)))))

;;This is a helper function.
;;It takes two list of unifiers in as arguments.
;;It will return a new list of unifiers.
;;In new list, for each elements in first arg,
;;append it with each elements in second arg one time.
(defun adding-lst (lst1 lst2)
  (labels ;;local function used to form pair
      ((single-add (s lst) 
	 (labels
	     ((append-lst (s1 s2) ;;recursively append each item in both lists
		(append  s1  s2)))
	   (cond
	     ((every #'atom s) 
	      (mapcar
	       #'(lambda (x) (append-lst (list s) (list x))) lst))
	     (t (mapcar
		 #'(lambda (x) (append-lst s (list x))) lst))))))
    (mapcan #' (lambda (x) (single-add x lst2)) lst1)))


;;This function takes two list in as arguments.
;;First arg is a list of variable, and second arg is a list of constants.
;;If input types wrong, it will  return nil.
;;Otherwise, it will return a list of unifiers,
;;that binding variables and constants with all possible ways. 
(defun distinct-bindings (varlst conlst)
  (cond
    ;;basic typecheck
    ((or (or (or (or (or (null varlst) (null conlst))  
		     (not (listp varlst))) (not (listp conlst)))
	     (notevery #'atom conlst))
	 (notevery #'atom varlst)) nil)
    ;;check first is variable list and second is constant list
    ((or (notevery #'var-chk varlst) (some #'var-chk conlst)) nil)
    ;;check if constants list is longer than ro equal to variable list
    ((< (list-length conlst) (list-length varlst)) nil)
    ;;type check correct
    (t (labels
	   ((binding-pair (s lst)
	      (list (mapcan #'(lambda (x)
				(list (append (list s) (list x)))) lst))))
	 ;;binding each variable seprately first
	 (let ((comblst (mapcan #'
			 (lambda (x) (binding-pair x conlst)) varlst)))
	   (cond ((equal 1 (list-length comblst)) (mapcar #'list (car comblst)))		 
		 (t(labels ;;then use this local function to append list
		       ((seprate-binding (clst comblst)
			  (cond
			    ((null comblst) clst)
			    (t (seprate-binding
				(adding-lst clst
					    (car comblst)) (cdr comblst))))))
		     ;;finally prune the unifiers
		     (prune-unifiers (seprate-binding (car comblst) (cdr comblst))))))))))) 

;;This is a helper function.
;;It takes one list as the only arguement in.
;;If this list is not a literal, it will return nil.
;;If this list is a negative literal, it will return its positive version.
;;If this list is a positive literal, it will return itself.
(defun check-literal (lst)
  (cond
    ((null lst) nil)
    ((not (listp lst)) nil)
    ((and (equal (car lst) 'not)
	  (or (not (listp (cadr lst)))
	      (or (notevery #'atom (cadr lst))
		  (or (some #'(lambda (x) (equal 'not x)) (cadr lst))
		      (every #'var-chk (cadr lst)))))) nil)
    ((and (not (equal (car lst) 'not))
	  (or (notevery #'atom lst)
	      (or (some #'(lambda (x) (equal x 'not)) (cdr lst))
		  (every #'var-chk lst))))nil)
    (t (cond
	 ((equal (car lst) 'not) (cadr lst))
	 (t lst)))))

;;This is a helper function.
;;It takes one list as the only arguement in.
;;If this list is not part of state, it will return nil.
;;Otherwise it returns true.
(defun check-state (lst)
  (cond
    ((null lst) nil)
    ((not (listp lst)) nil)
    ((notevery #'atom lst) nil)
    ((some #'(lambda (x) (equal 'not x)) lst) nil)
    ((some #'var-chk lst) nil)
    (t t)))

;;This is a helper function.
;;It takes two lists in as arguments.
;;It returns nil if their type is not list of atoms
;;Otherwise it returns unifer result of two coming lists
(defun unifier (lst1 lst2)
  (cond
    ;;type check
    ((not (listp lst1))nil)
    ((not (listp lst2))nil)
    ((notevery #'atom lst1)nil)
    ((notevery #'atom lst2)nil) 
    ;;baisc case: indentical
    ((equal lst1 lst2) T)
    ;;basic case: length not equal
    ( (not (equal (list-length lst1) (list-length lst2))) nil)
    (t
     (block nil
       ;;use hashtable
       (let ((ht (make-hash-table)))
	 ;;local function
	 (labels
	     ((chklst (s1 s2)
		(if  (var-chk s1) ;; it has to be a variable
		     (let ((val (gethash s1 ht)))
		       (if (null val) ;; didn't find in hashtable
			   (setf(gethash s1 ht) s2)
			   (if (not (equal val s2))
			       (return nil))))
		     (if (not (equal s1 s2))
			 (return nil)))))
	   (mapcar #' chklst lst1 lst2);; build hash-table
	   ;;get output
	   (loop for key being the hash-keys of ht
	      collect (list key (gethash key ht)))))))))

;;This function takes three arguements.
;;First arg is a literal.
;;Second arg is a state.
;;Third arg is a list of constants.
;;If the type of any arguements is wrong, return nil.
;;If first arg is postive literal with no variable,
;;it will return true or nil, according to whether it is true in the state.
;;If first arg is negtive literal with no variable,
;;it will return true or nil, according to wheter it fails in the state.
;;If first arg is positive with some variables,
;;it will return unifiers according to the predicate.
;;If first arg is negtive with some variables,
;;it will return distinct bindings, but without what happend in the previous case.
(defun literal-unifiers (ltr sttlst conslst)
  (cond
    ;;typecheck
    ((or (null sttlst) (not (listp sttlst))) nil)
    ((not (and (check-literal ltr)
	       (every #'check-state sttlst))) nil)
    ((or(or (or (null conslst)
		(not (listp conslst)))
	    (notevery #'atom conslst))
	(some #'var-chk conslst))nil)
    ;;build hashtable to do optimization
    (t (let* ((ht (make-hash-table :test 'equal)) 
	      (nltr (check-literal ltr)))
	 (labels
	     ((bld-hash (stt)
		(let ((val (gethash (car stt) ht)))
		  (if (null val)
		      (setf (gethash (car stt) ht) (list stt))
		      (setf (gethash (car stt) ht) (append val (list stt)))))))	   
	   (mapcar #'bld-hash sttlst))
	 ;;local function to extract the consants in each literal of state.
	 (labels
	     ((extract-cons (stt nltr ret)
		(cond
		  ((null nltr) ret)
		  ((var-chk (car nltr))
		   (extract-cons (cdr stt) (cdr nltr) ;; recursively extract
				 (append ret (list (car stt)))))
		  (t (extract-cons(cdr stt) (cdr nltr) ret)))))
	   (cond
	     ;;neg ltr
	     ((equal (car ltr) 'not)
	      ;;no variable
	      (cond ((check-state nltr)
		     (cond ((null
			     (find nltr (gethash (car nltr) ht) :test #'equal)) t)
			   (t nil)))
		    ;;with variable
		    (t (let* ((clst (mapcar #'(lambda (x) (extract-cons x nltr nil))
					    (gethash (car nltr) ht)))
			      (varlst (mapcan #'
				       (lambda (x)
					 (member-if #'var-chk (list x))) nltr))
			      ;;do distinct-bindings
			      (dislst (distinct-bindings varlst conslst))
			      ;;do unifier 
			      (samelst (mapcar #'
					(lambda (x)
					  (reverse (unifier varlst x))) clst)))
			 (labels
			     ((remove-same (dis samelst)
				(cond
				  ((find dis samelst :test #'equal) nil)
				  (t dis))))
			   ;;take unifier from distinct-bindings
			   (remove 'nil (mapcar #'
					 (lambda (x)
					   (remove-same x samelst)) dislst)))))))
	     
	     ;;pos ltr no variable
	     (t (cond ((check-state nltr)
		       (cond ((find nltr (gethash (car nltr) ht) :test #'equal) t)
			     (t nil)))
		      ;;with variable
		      (t (let* ((clst (mapcar #'
				       (lambda (x) (extract-cons x nltr nil))
				       (gethash (car nltr) ht)))
				(varlst (mapcan #'
					 (lambda (x)
					   (member-if #'var-chk (list x))) nltr)))
			   ;;use unifier function
			   (mapcar #'(lambda (x) (unifier varlst x)) clst)))))))))))


;;This is a helper function.
;;It takes two list in as arguements,which first is unified list and second is literal.
;;It returns nil if args types wrong.
;;Otherwise it returns a new literal with the variable replaced by constant
;;if it can be found in unified list
(defun apply-unifier (ulst lst)
  (cond
    ((not(check-unifier ulst)) nil) ;;call function to check if it is unifier
    ((not(listp lst))nil)
    ((notevery #'atom lst) nil) ;;check if it is literal
    ((null ulst) lst) ;;recursively apply-unifier
    (t (subst (second (car ulst)) (first (car ulst));; use subst
	      (apply-unifier (cdr ulst) lst)))))

;;This is a helper function.
;;It takes two list as arguements.
;;It will append each item in first arg by each item in second arg respectively.
(defun append-check (lsta lstin)
  (labels 
      ((append-lst (lsta in) ;;local function to do appending
	 (labels
	     ((append-s  (a in)
		(let*  ((lstappend  (remove-duplicates (append in a) :test #'equal))
			(varlsta (mapcar #'car lstappend)))
		(cond
		  ;;if the item in second already exit in first, then don't append
		  ((notevery #'null
				(mapcar #' (lambda (x)
					     (member x (cdr (member x varlsta)))) varlsta))nil)
		  (t lstappend)))))
	   (remove 'nil (mapcar #'(lambda (x) (append-s x in)) lsta)))))
    (mapcan #' (lambda (x) (append-lst lsta x)) lstin)))


;;This is a helper function.
;;It takes one list as the second argument, and first arguement always empty.
;;It recursively appends the first arg until second is reach to the end.
(defun recur-check (lsta lstr)
  (cond
    ((null lstr) lsta)
    (t (recur-check (append-check (remove-duplicates lsta :test #'equal)  (car lstr)) (cdr lstr)))))

;;This is a helper function.
;;It takes three list in as arguments.
;;First one is a list of preconds.
;;Second is state.
;;Third is a list of constants.
;;If type of any arg is wrong, it will return nil.
;;Otherwise return all bindings that make them true in state.
(defun preconds-instances (preconds state constants)
  (labels ;;local function to check each precond true in state bindings.
      ((precond-check (precond conslst)
	 (let ((varlst (mapcan #'(lambda (x)
				   (member-if #'var-chk (list x))) precond)))
	   (cond ((not (null varlst))
		  (let* ((dislst (distinct-bindings varlst constants));;find distinct first
			 (pcapplst (mapcar #'(lambda (x)
					       (list (apply-unifier x precond) x))
					   dislst)))
		    (labels ;; use this local function find if any true in state
			((findtrue (pcapp state)
			   (cond ((find (first pcapp) state :test #'equal) (second pcapp))
				 (t nil))))
		      (remove 'nil (mapcar #'(lambda (x) (findtrue x state)) pcapplst)))))
		 (t nil)))))
    ;;assign local variable that include all true bindings.
    (let* ((opeff (remove 'nil (mapcar #'(lambda (x) (precond-check x constants)) preconds)))
	   (opappend (recur-check (car opeff) (cdr opeff))))
      (prune-unifiers opappend))))

;;This is a helper function.
;;It takes three list in as arguments.
;;First one is a list of bindings (unifiers).
;;Second is list of preconds.
;;Third is a state.
;;If the unifiers apply to preconds make it all true in state, then it will return this set of unifiers.
;;Otherwise it will return nil.
(defun final-check-op (blst preconds state)
  (let ((preconda (mapcar #'(lambda (x) (apply-unifier blst x)) preconds)))
    (cond
      ((every #'(lambda (x) (find x state :test #'equal)) preconda) blst)
      (t nil))))


;;This function takes two arguements.
;;First is an operation. second is a state.
;;If type of any arg is wrong, return nil.
;;Otherwise call precond-instantces and get the same return.
(defun operator-instances (op state)
  (let* ((opname (first op))
	 (varlst (second op))
	 (precond (third op))
	 (effect (fourth op)))
    ;;type check
    (cond
      ((not (atom opname)) nil)
      ((not (every #'var-chk varlst))nil)
      ((some #'null (mapcar #'check-literal precond)) nil)
      ((some #'null (mapcar #'check-literal effect)) nil)
      ((some #'null (mapcar #'check-state state)) nil)
      (t (labels ;; use this function to extract constants
	     ((extract-constants (lst) 
		(mapcar #' (lambda (x)
			     (find-if-not #'var-chk (list x)))(cdr lst))))
	   (let ((constants
		  (remove-duplicates
		   (append
		    (append (mapcan #'extract-constants
				    (check-literal precond))
			    (mapcan #'extract-constants
				    (check-literal effect)))
		    (mapcan #'extract-constants state)))))
	     ;;call helper function
	     (remove 'nil (mapcar #'
	      (lambda (x) (final-check-op x precond state))
		      (remove-duplicates
		       (preconds-instances precond state constants) :test #'equal)))))))))
