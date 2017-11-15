;;global hashtable to store the transition
(setq ht (make-hash-table :test 'equal))

;;This function takes three arguments in.
;;First is a state, second is an operation, third is another state.
;;This function aims to update global hashtale.
;;Create/update hash-table entry with key s.
;;Each entry should looks like (((prev o)) s ((next o)))
;;It will return nil if s is not new, otherwise return s.
;;This function use hashtable.
(defun store-transition (r o s)
  ;;assign local variales
  (let* ((ss (sort s #'< :key #'sxhash)) ;; sort state
	 (vals (gethash ss ht))
	 (valr (gethash r ht)))
    ;;type check
    (cond
      ((or (or (notevery #'check-state r)
	       (notevery #'check-state s))
	   (not (check-state o))) nil)
      ;;update hashtable
      (t (if (null vals)
	     ;;if the state is new
	    (progn
	      (setf (gethash ss ht) (list (list (list r o))  ss nil))
	      (setf (gethash r ht) (list (first valr) (second valr)
					 (append (third valr)
						 (list (list ss o)))))
	      (return-from store-transition ss))
	    ;;if state not new
	    (progn
	      (setf (gethash ss ht) (list (append (first vals)
						  (list (list r o)))
					  (second vals)(third vals)))
	      (return-from store-transition nil)))))))

;;This function takes three arguments in.
;;Firsst is a state, second is an operator, third is an operator instance.
;;This function aims to use operator and operator instances to gernate
;;the successor state of the given state.
;;This function also call store-transition to update hash-table
;;It wil exactly what store-transition returns.
;;This function local defined function and call store-transition
(defun successor-state (s op opin)
  ;;define local function to remove gound literal not in s
  (labels
      ((remove-state (neg s ret)
	 (cond
	   ((null s) ret)
	   ((null (find (car s) neg :test 'equal))
	    (remove-state neg (cdr s) (append ret (list (car s)))))
	   (t (remove-state neg (cdr s) ret)))))
    ;;assign local variable
    (let* ((opname (first op))
	   (varlst (second op))
	   (preconds (third op))
	   (effect (fourth op))
	   (varapp (apply-unifier opin varlst))
	   (act (append (list opname) varapp)) 
	   (effapp (mapcar #'(lambda (x) (apply-unifier opin x)) effect))
	   (neg-pos (seperate-neg-pos effapp '() effapp))
	   (neg (mapcar #'second (first neg-pos)))
	   (suc1 (remove-state neg s '()))
	   (suc (remove-duplicates (append suc1 (second neg-pos))
				   :test 'equal)))
      (cond
	;;type check
	((and (and (check-operator op)
		   (check-unifier opin))
	      ;;call store-transition to generate new state and update hash
	      (every #'check-state s)) (store-transition s act suc))
	(t nil)))))

;;This function takes two arguments in.
;;First one is a state, second one is a list of operators.
;;This function calls operator-instances to generate instances
;;and then pass these instances, operator and state to successor-state function
;;to update hash-table and see what is new state
;;It will call successor-state several times
;;and combine the return result from them and return them.
;;It calls opoerator-instances and successor-state.
(defun successor-states (s ops)
  (labels
      ((seperate-ops (opinp)
	 (cond
	   ((null (second opinp)) nil)
	   (t(mapcar #'(lambda (x) (list (first opinp) x)) (second opinp))))))
    (let* ((opinsp (mapcar #' (lambda (x) (list x (operator-instances x s)))
			      ops))
	   (opinsps (mapcan #' seperate-ops opinsp)))
      (cond
	((and (notevery #'check-state s )
	      (notevery #'check-operator ops)) nil)
	(t(remove nil (mapcar #' (lambda (x)
				  (successor-state s (first x)
						   (second x))) opinsps)))))))
;;This function takes two arguments in.
;;Both of them a state
;;If second state is contained in first state,
;;then return true, otherwise null.   
(defun check-goal (s gcond)
  ;;defind local variabl
  (let ((pre-neg-pos (seperate-neg-pos gcond '() gcond)))
    ;;check if find goal
    (cond
      ((and
	(or (notany #'null (mapcar
			    #'(lambda (x)
				(find x s :test #'equal)) (second pre-neg-pos)))
	    (null (second pre-neg-pos)))
	(or (every #'null (mapcar
			   #'(lambda (y)
			       (find y s :test #'equal)) (first pre-neg-pos)))
	    (null (first pre-neg-pos)))) t)
      (t nil))))

;;This function takes trhee arguements in.
;;First is a list of operators, second is state, third is state.
;;This function aims to find what operations need to take,
;;to achieve second state from first state.
;;It will return "solution not found", if no such operations combination.
;;If search of operations combination takes too long time,
;;return "Search takes too long time.",
;;othersie return the list of operations.
;;This function calls make-open-list and back-track.
(defun find-plan (ops is gcond)
  ;;typecheck
  (cond
    ((and (and (notevery #'check-operator ops)
	       (notevery #'check-state is))
	  (notevery #'check-state gcond)) nil)
    ;;if no steps needed return true
    ((check-goal is gcond) t)
    ;;make open list 
    (t (let ((so (make-open-list (list is) ops gcond 0)))
	 (cond
	   ;;if no solution
	   ((null so) '"Solution not found!")
	   ;;if search takes too long time
	   ((equal so '"Search takes too long time.") so)
	   ;;do backtrack
	   (t (reverse (back-track so is '())))))))) 

;;This function takes one argument in.
;;It should be a operator
;;This function aims to do type check if input is correct operator.
;;It returns true if correct, otherwise null.
(defun check-operator (op)
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
      (t t))))

;;This functino takes two argument in.
;;First is a initial state, second is the state try to match the first one.
;;If the state matches, return true, otherwise nil.
(defun find-init (init prev)
  (cond
    ((null prev)nil)
    ((equal init (first (car prev))) (car prev))
    (t (find-init init (cdr prev)))))

;;This function takes trhee artuments in.
;;First is a state, second is a initial state,second is what will be return.
;;It should be a list of operations
;;This function aims to recursively track the operations in the hash-table
;;by using state and its previous state.
;;It will return a list if the list of operations if find-init return ture,
;;otherwise nil.
;;It calls find-init and use recursion.
(defun back-track (state init ret)
  ;;define local variable
  (let* ((prev (first (gethash state ht)))
	(findchk (find-init init prev)))
    (cond
      ;;check if not find initial state then go further
      ((not (null findchk)) (append ret  (cdr findchk)))
      (t (let ((pick (car prev))) 
	   (if (null findchk)
	       (progn
		 ;; recursively track
		 (setf (gethash state ht) (cdr prev))
		 (back-track (first pick) init
			     (append ret (cdr pick)))))))))) 
    
;;This function takes two arguements in.
;;First is a list of state, second is a state.
;;This function aims to find if first list contains second states.
;;If yes, it will return true, otherwise return nil.	
(defun find-goal (news gcond)
  (cond
    ((null news) nil)
    ((check-goal (car news) gcond) (car news))
    (t (find-goal (cdr news) gcond))))

;;This function takes four arguements in.
;;First is a list of state, second is a list of operators,
;;third is a conditions, fourth is a number.
;;This function try to call successor-states to generate new states
;;and use them to check if there are goal conditions.
;;If not, recursivcely go on to find it.
;;If the fun is exaughstive, return nil.
;;If the number track recursion times treater than 1000,
;;reutrn "Search takes too long time.",
;;otherwise return the state found satisfied goal conditions.
;;This function use successor-states and find-goal, also use recursion.
(defun make-open-list (open-list ops gcond count)
  (cond
    ((null open-list) nil)
    (t (let* ((news (successor-states (car open-list) ops))
	      (chkret (find-goal news gcond))
	      (rmmo (cdr open-list)))
	 (cond
	   ((equal count 10000) '"Search takes too long time.")
	   ((null chkret) (make-open-list (append rmmo news)
					  ops gcond (+ count 1)))
	   (t chkret))))))

;;This function takes three arguments in.
;;First should be a list of literal, second should be intially empty,
;;third should be intially the same as first one.
;;This function recursely seperate the ground
;;and not ground literals in the first argument.
;;return a new list with them seperate.
;;This function use recursion.
(defun seperate-neg-pos (eff neg pos)
  (cond
    ((null eff) (list neg pos))
    ((equal (first (car eff)) 'not)
     (seperate-neg-pos (cdr eff) (append neg (list (car eff)))
		       (remove (car eff) pos)))
    (t (seperate-neg-pos(cdr eff) neg pos))))

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
					     (member x (cdr (member x conlst))))
					   conlst))
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
		     (prune-unifiers (seprate-binding (car comblst)
						      (cdr comblst))))))))))) 

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
		  (let* ((dislst (distinct-bindings varlst constants))
			 ;;find distinct first
			 (pcapplst (mapcar #'(lambda (x)
					       (list (apply-unifier x precond)
						     x))
					   dislst)))
		    (labels ;; use this local function find if any true in state
			((findtrue (pcapp state)
			   (cond ((find (first pcapp) state :test #'equal)
				  (second pcapp))
				 (t nil))))
		      (remove 'nil (mapcar #'(lambda (x)
					       (findtrue x state)) pcapplst)))))
		 (t nil)))))
    ;;assign local variable that include all true bindings.
    (let* ((opeff (remove 'nil (mapcar #'(lambda (x) (precond-check x constants))
				       preconds)))
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
      ((not (check-operator op)) nil)
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
		       (preconds-instances precond state constants)
		       :test #'equal)))))))))
  
  
    
    
