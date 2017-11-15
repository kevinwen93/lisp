;;helper function
;;take a atom in and
;;return true or nil if it is a variable
(defun var-chk (s)
  (cond
    ;;if variable
    ((not (equal (write-to-string s)
		 (string-left-trim "?" (write-to-string s)))) t)
    ;;constant
    (t nil)))

;;take two list in
;;return nil if their type is not list of atoms
;;return unifer result of two coming list if type correct
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

;;helper function
;;take a list in
;;return true if it is unified list
;;otherwise nil
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
	  (t t))))))

;;take two list in, one is unified list and another is literal
;;return nil if args types wrong
;;return a new literal with the variable replaced by constant if it can
;;be found in unified list
(defun apply-unifier (ulst lst)
  (cond
    ((not(check-unifier ulst)) nil) ;;call function to check if it is unifier
    ((not(listp lst))nil)
    ((notevery #'atom lst) nil) ;;check if it is literal
    ((null ulst) lst) ;;recursively apply-unifier
    (t (subst (second (car ulst)) (first (car ulst));; use subst
	      (apply-unifier (cdr ulst) lst)))))

;;take a list of literal or states in
;;return nil if args is not literal or state
;;otherwise return a list of non-redundant constant
(defun extract-constants (ltr-lst)
  (block nil
    (cond
      ;;do typecheck first
    ((not (listp ltr-lst))nil) ;; not a list
    ((notevery #'listp ltr-lst)nil);; not a list of list
    ;;literal has more than 2 items
    ((notevery #'(lambda (x) (<= '2 (list-length x))) ltr-lst) nil)
    ;;local function to treat single literal 
    (t (labels
	   ((extract-lst (lst)
	      (labels
		  ;;loacal function to treat each item see if constant
		  ((extract-cons (s)
		     (cond
		       ((not (var-chk s))(list s))
		       (t nil))))
		(cond
		  ;;type check
		  ((and (not (equal 'not (car lst))) ;; if not negative
		    (or (some #' listp lst) ;; scoop check
			;; can't be all not
			(or (every #'(lambda (x) (equal x 'not)) lst)
			 ;; first one must be constant  
			  (var-chk (car lst)))))
		   (return nil))
		  ((and (equal 'not (car lst)) ;; if negative 
			(or (notany #'listp lst) ;;scoop check
			    (or (not (equal '2 (list-length lst)))
				 ;;can't be all not 
			       (or (every #'(lambda (x)
					      (equal x 'not)) (cadr lst))
				;;more than one item in literal
				(or (equal '1 (list-length (cadr lst)))
					;;first must be constant
					(or (var-chk (caadr lst))
				  ;; no more list in list 
					    (notevery #'atom (cadr lst))))))))
		   (return nil))
		  ;;if negative do extract
		  ((and (and (equal 'not (car lst))
			(some #' listp lst)) (every #'atom (cadr lst)))
		   (mapcan #'extract-cons (cadr lst)))
		  ;;positive do extract
		  (t (mapcan #'extract-cons lst))))))
	 ;;append all together
	 (remove-duplicates (mapcan #'extract-lst ltr-lst)))))))

;; take a list of unified list in
;;reutrn nil if arg type wrong
;;otherwise return a new listof unified list without which unified list
;;binding two variable to the same constant
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
      
		  
  
