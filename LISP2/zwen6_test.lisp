(defun result-format2 (rslt input1 input2 crrc_rslt)
  (format t "~:[FAIL~;pass~] ... Input: ~d and ~d Expected: ~d~%"
	  rslt input1 input2  crrc_rslt))

(defun result-format3 (rslt input1 input2 input3 crrc_rslt)
  (format t "~:[FAIL~;pass~] ... Input: ~d, ~d and ~d Expected: ~d~%"
	  rslt input1 input2 input3  crrc_rslt))

(defun func-format (funcname)
  (format t "~%Runing test for ~d~%" funcname))
  
(defun cpar-rslt (func_rslt  crrc_rslt)
  (equal func_rslt crrc_rslt))

(load "zwen6_src.lisp")

(setq tset-ie '(nil nil nil nil nil nil nil nil nil nil nil nil))

;;testcase for distinct-bindings, including invalid inputs.
(func-format "distinct-bindings")
(setq tset1-1 '(nil (?x ?y) (?x a) (?x ?y ?z) (?x ?y) (?x ?y ?z)))
(setq tset1-2 '((a b) nil (a b) (a b) (a b) (a b c table )))
(setq tset1-e '(nil nil nil nil (((?X A) (?Y B)) ((?X B) (?Y A)))
		(((?X A) (?Y B) (?Z C)) ((?X A) (?Y B) (?Z TABLE))
		 ((?X A) (?Y C) (?Z B)) ((?X A) (?Y C) (?Z TABLE))
		 ((?X A) (?Y TABLE) (?Z B)) ((?X A) (?Y TABLE) (?Z C))
		 ((?X B) (?Y A) (?Z C)) ((?X B) (?Y A) (?Z TABLE))
		 ((?X B) (?Y C) (?Z A)) ((?X B) (?Y C) (?Z TABLE))
		 ((?X B) (?Y TABLE) (?Z A)) ((?X B) (?Y TABLE) (?Z C))
		 ((?X C) (?Y A) (?Z B)) ((?X C) (?Y A) (?Z TABLE))
		 ((?X C) (?Y B) (?Z A)) ((?X C) (?Y B) (?Z TABLE))
		 ((?X C) (?Y TABLE) (?Z A))  ((?X C) (?Y TABLE) (?Z B))
		 ((?X TABLE) (?Y A) (?Z B)) ((?X TABLE) (?Y A) (?Z C))
		 ((?X TABLE) (?Y B) (?Z A)) ((?X TABLE) (?Y B) (?Z C))
		 ((?X TABLE) (?Y C) (?Z A)) ((?X TABLE) (?Y C) (?Z B)))))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'distinct-bindings
				       tset1-1 tset1-2) tset1-e)
	   tset1-1 tset1-2 tset1-e)

;;testcases for literal-unifiers
(func-format "literal-unifiers")
;;invalid inputs
(setq tset2-i1c '((on a b) (on a b) (on a b) (on a b)
		  (on a b) (on a b) (on a b) (on a b)
		  (on a b) (on a b) (on a b) (on a b)))
(setq tset2-i2c (mapcar #'list tset2-i1c))
(setq tset2-i3c '((a b table) (a b table) (a b table)
		  (a b table) (a b table) (a b table)
		  (a b table) (a b table) (a b table)
		  (a b table) (a b table) (a b table))) 
(setq tset2-i1 '(nil a (a (on a b)) ((a) (on a b)) ((on (a b)))
		 ((on a b) (?x b c)) ((not a)) ((not))
		 ((not (?x a))) ((not (on (a b))))))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'literal-unifiers
				       tset2-i1 tset2-i2c tset2-i3c)
		   tset-ie)
	   tset2-i1 tset2-i2c tset2-i3c tset-ie)
(setq tset2-i2 '(nil a (a (on a b)) ((not (on a b))) ((on a ?x))))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'literal-unifiers
				       tset2-i1c tset2-i2 tset2-i3c)
		   tset-ie)
	   tset2-i1c tset2-i2 tset2-i3c tset-ie)
(setq tset2-i3 '(nil a (a '(on b)) (a b ?x)))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'literal-unifiers
				       tset2-i1c tset2-i2c tset2-i3)
		   tset-ie)
	   tset2-i1c tset2-i2c tset2-i3 tset-ie)
;;normal testcase 
(setq tset2-1 '((on a b) (on b c) (not (on a b)) (not (on b c))
		(on ?y ?z) (on a ?z) (not (on ?y ?z)) (not (on a ?z))
		(clear ?x) (not (clear ?x))))
(setq tset2-2 '(((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))
		((on A B) (on B Table) (clear a))))
(setq tset2-3 '((A B Table) (A B Table) (A B Table) (A B Table)
		(A B Table) (A B Table) (A B Table) (A B Table)
		(A B Table) (A B Table)))
(setq tset2-e '(t nil nil t (((?Z B) (?Y A)) ((?Z TABLE) (?Y B)))
		(((?Z B)) ((?Z TABLE))) (((?Y A) (?Z TABLE)) ((?Y B) (?Z A))
					 ((?Y TABLE) (?Z A)) ((?Y TABLE) (?Z B)))
		(((?Z A))) (((?x a))) (((?X B)) ((?X TABLE)))))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'literal-unifiers
				       tset2-1 tset2-2 tset2-3)
		   tset2-e)
	   tset2-1 tset2-2 tset2-3 tset2-e)

;;testcases for operator-instances
(func-format "operator-instances")
;;invalid input
(setq tset3-i2c '((move-to-table (?x ?y) ((on ?x ?y) (clear ?x))
		   ((on ?x Table) (clear ?y) (not (on ?x ?y))))
		  '(move-to-table (?x ?y) ((on ?x ?y) (clear ?x))
		    ((on ?x Table) (clear ?y) (not (on ?x ?y))))
		  '(move-to-table (?x ?y) ((on ?x ?y) (clear ?x))
		    ((on ?x Table) (clear ?y) (not (on ?x ?y))))
		  '(move-to-table (?x ?y) ((on ?x ?y) (clear ?x))
		    ((on ?x Table) (clear ?y) (not (on ?x ?y))))
		  '(move-to-table (?x ?y) ((on ?x ?y) (clear ?x))
		    ((on ?x Table) (clear ?y) (not (on ?x ?y))))))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'operator-instances
				       tset3-i2c tset2-i2) tset-ie)
	   tset3-i2c tset2-i2 tset-ie)
;;normal testcases
(setq tset3-1 '((move-to-table (?x ?y)
                      ((on ?x ?y) (clear ?x))
                      ((on ?x Table) (clear ?y) (not (on ?x ?y))))))
(setq tset3-2 '(((on A B) (on B Table) (clear A))))
(setq tset3-e  '((((?x A) (?y B)))))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'operator-instances
				       tset3-1 tset3-2) tset3-e)
	   tset3-1 tset3-2 tset3-e)

;;given domain testcases
(defparameter ferry-operators
  '((board (?x ?y)
     ;; Preconds
     ((auto ?x)
      (place ?y)
      (at ?x ?y)
      (at-ferry ?y)
      (empty-ferry))
     ;; Effects
     ((on ?x Ferry)
      (not (at ?x ?y))
      (not (empty-ferry))))

    (sail (?x ?y)
     ;; Preconds
     ((place ?x)
      (place ?y)
      (at-ferry ?x))
     ;; Effects
     ((at-ferry ?y)
      (not (at-ferry ?x))))

    (debark (?x ?y)
     ;; Preconds
     ((auto ?x)
      (place ?y)
      (on ?x Ferry)
      (at-ferry ?y))
     ;; Effects
     ((not (on ?x Ferry))
      (at ?x ?y)
      (empty-ferry)))))

(defparameter ferry-inits
  '(((place a) (place b) (auto c1) (auto c2)
     (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))
    ((place a) (place b) (auto c1) (auto c2)
     (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))
    ((place a) (place b) (auto c1) (auto c2)
     (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))

(defparameter ferry-answer
  '((((?Y A) (?X C1)) ((?Y A) (?X C2)))
    (((?Y B) (?X A)))
    nil))

(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'operator-instances
				       ferry-operators ferry-inits) ferry-answer )
	   ferry-operators ferry-inits ferry-answer)


(defparameter hanoi-operators
  '((move-disk (?disk ?below-disk ?new-below-disk)
     ;; Preconds
     ((disk ?disk)
      (smaller ?disk ?new-below-disk) ; Handles pegs
      (on ?disk ?below-disk)
      (clear ?disk)
      (clear ?new-below-disk))
     ;; Effects
     ((clear ?below-disk)
      (on ?disk ?new-below-disk)
      (not (on ?disk ?below-disk))
      (not (clear ?new-below-disk))))))
      
;; 3 disks, all initially on P1
(defparameter hanoi-inits
  '(((smaller D1 P1) (smaller D2 P1) (smaller D3 P1)
    (smaller D1 P2) (smaller D2 P2) (smaller D3 P2)
    (smaller D1 P3) (smaller D2 P3) (smaller D3 P3)
    (smaller D1 D2) (smaller D2 D3)
    (clear P2) (clear P3) (clear D1)
    (disk D1) (disk D2) (disk D3)
     (on D1 D2) (on D2 D3) (on D3 P1))))

(defparameter hanoi-answer
  '((((?BELOW-DISK D2) (?NEW-BELOW-DISK P2) (?DISK D1))
     ((?BELOW-DISK D2) (?NEW-BELOW-DISK P3) (?DISK D1)))))

(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'operator-instances
				       hanoi-operators hanoi-inits) hanoi-answer )
	   hanoi-operators hanoi-inits hanoi-answer)


