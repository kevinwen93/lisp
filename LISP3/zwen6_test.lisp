;;some test domains
(setq ferry-operators
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

(setq ferry-inits
  '((place a) (place b) (auto c1) (auto c2)
    (at c1 a) (at c2 a) (at-ferry a) (empty-ferry)))

(setq ferry-goals
  '((at c1 b)
    (at c2 b)))

  
;; Towers of Hanoi Domain
;; 3 pegs, 3 disks
      
;; Remember that parameters are distinct by assumption.
      
(setq hanoi-operators
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
(setq hanoi-inits
  '((smaller D1 P1) (smaller D2 P1) (smaller D3 P1)
    (smaller D1 P2) (smaller D2 P2) (smaller D3 P2)
    (smaller D1 P3) (smaller D2 P3) (smaller D3 P3)
    (smaller D1 D2) (smaller D2 D3)
    (clear P2) (clear P3) (clear D1)
    (disk D1) (disk D2) (disk D3)
    (on D1 D2) (on D2 D3) (on D3 P1)))
      
;; Goal: 3 disk on P3
(setq hanoi-goals
  '((on D1 D2) (on D2 D3) (on D3 P3)))


;; Monkey Domain

(setq monkey-operators
  '((go-to (?x ?y) ;; Go to x from y.
     ;; Preconds
     ((location ?x)
      (location ?y)
      (on-floor)
      (at Monkey ?y))
     ;; Effects
     ((at Monkey ?x)
      (not (at Monkey ?y))))

    (climb (?x)
     ;; Preconds
     ((location ?x)
      (at Box ?x)
      (at Monkey ?x))
     ;; Effects
     ((on-box ?x)
      (not (on-floor))))

    (push-box (?x ?y)
     ;; Preconds
     ((location ?x)
      (location ?y)
      (at Box ?y)
      (at Monkey ?y)
      (on-floor))
     ;; Effects
     ((at Monkey ?x)
      (not (at Monkey ?y))
      (at Box ?x)
      (not (at Box ?y))))

    (get-knife (?y)
     ;; Preconds
     ((location ?y)
      (at Knife ?y)
      (at Monkey ?y))
     ;; Effects
     ((has-knife)
      (not (at Knife ?y))))

    (grab-bananas (?y)
     ;; Preconds
     ((location ?y)
      (has-knife)
      (at Bananas ?y)
      (on-box ?y))
     ;; Effects
     ((has-bananas)))
     
    (pick-up-glass (?y)
     ;; Preconds
     ((location ?y)
      (at Glass ?y)
      (at Monkey ?y))
     ;; Effects 
     ((has-glass)
      (not (at Glass ?y))))
     
    (get-water (?y)
     ;; Preconds
     ((location ?y)
      (has-glass) 
      (at Waterfountain ?y)
      (at Monkey ?y)
      (on-box ?y))
     ;; Effects
     ((has-water)))))
      
(setq monkey-inits-1
  '((location P1) (location P2) (location P3) (location P4)
    (at Monkey P1) (on-floor) (at Box P2) (at Bananas P3)
    (at Knife P4)))
      
(setq monkey-goals-1
  '((has-bananas)))
     
(setq monkey-inits-2
  '((location P1) (location P2) (location P3) (location P4) (location P5)
    (at Monkey P1) (on-floor) (at Box P2) (at Bananas P3) (at Knife P4)
    (at Waterfountain P3) (at Glass P5)))

(setq monkey-goals-2
    '((has-bananas) (has-water)))


(load "zwen6_src.lisp")

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

;;testcases for store-transition
(func-format "store-transition")
(setq test1-1 '(((on a b) (on ?x b))
		((on a b) (on b table))
		((on a b) (on b table))
		((on a b) (on b table))))
(setq test1-2 '((move a b)
		(move a ?x)
		(move a b)
		(move a b)))
(setq test1-3 '(((clear a))
		((clear a))
		((clear ?x))
		((clear a))))
(setq test1-e '(nil nil nil ((clear a))))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'store-transition
				       test1-1 test1-2 test1-3)
		   test1-e)
	   test1-1 test1-2 test1-3 test1-e)
		
;;testcases for successor-state
(load "zwen6_src.lisp")
(func-format "successor-state")
(setq opin (first (operator-instances '(board (?x ?y)
     ;; Preconds
     ((auto ?x)
      (place ?y)
      (at ?x ?y)
      (at-ferry ?y)
      (empty-ferry))
     ;; Effects
     ((on ?x Ferry)
      (not (at ?x ?y))
      (not (empty-ferry)))) '((place a) (place b) (auto c1) (auto c2)
    (at c1 a) (at c2 a) (at-ferry a) (empty-ferry)))))
(setq test2-1 '(((on a b) (on ?x b))
	        ((place a) (place b) (auto c1) (auto c2)
		 (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))
		((place a) (place b) (auto c1) (auto c2)
		 (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))
(setq test2-2 '((board (?x ?y)
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
		(move-to-table (?x a)
		 ((on ?x ?y) (clear ?x))
		 ((on ?x Table) (clear ?y) (not (on ?x ?y))))
	        (board (?x ?y)
		 ;; Preconds
		 ((auto ?x)
		  (place ?y)
		  (at ?x ?y)
		  (at-ferry ?y)
		  (empty-ferry))
		 ;; Effects
		 ((on ?x Ferry)
		  (not (at ?x ?y))
		  (not (empty-ferry))))))
(setq test2-3 '(opin opin nil))
(setq test2-e '(nil nil nil))
(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'successor-state
				       test2-1 test2-2 test2-3)
		   test2-e)
	   test2-1 test2-2 test2-3 test2-e)
(load "zwen6_src.lisp")
(setq test2-1 '(((place a) (place b) (auto c1) (auto c2)
		 (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))
(setq test2-2 '((board (?x ?y)
		 ;; Preconds
		 ((auto ?x)
		  (place ?y)
		  (at ?x ?y)
		  (at-ferry ?y)
		  (empty-ferry))
		 ;; Effects
		 ((on ?x Ferry)
		  (not (at ?x ?y))
		  (not (empty-ferry))))))
(setq test2-3 '(opin))
(setq test2-3 '(((PLACE B) (PLACE A) (AT-FERRY A)
		 (ON C1 FERRY) (AT C2 A) (AUTO C1) (AUTO C2))))

(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'successor-state
				       test2-1 test2-2 test2-3)
		   test2-e)
	   test2-1 test2-2 test2-3 test2-e)


;;testcases for successor-states
(load "zwen6_src.lisp")
(func-format "successor-states")
(setq test3-1 '(((?x)) ((place a) (place b) (auto c1) (auto c2)
			(at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))      	
(setq test3-2 '(((board (?x ?y)
		 ;; Preconds
		 ((auto ?x)
		  (place ?y)
		  (at ?x ?y)
		  (at-ferry ?y)
		  (empty-ferry))
		 ;; Effects
		 ((on ?x Ferry)
		  (not (at ?x ?y))
		  (not (empty-ferry)))))
		((move-to-table (?x a)
		  ((on ?x ?y) (clear ?x))
		  ((on ?x Table) (clear ?y) (not (on ?x ?y)))))))
(setq test3-e '(nil nil))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'successor-states
				       test3-1 test3-2) test3-e)
	   test3-1 test3-2 test3-e)
(load "zwen6_src.lisp")
(setq test3-1 '(((place a) (place b) (auto c1) (auto c2)
	   (at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))
(setq test3-2 '(((board (?x ?y)
		 ;; Preconds
		 ((auto ?x)
		  (place ?y)
		  (at ?x ?y)
		  (at-ferry ?y)
		  (empty-ferry))
		 ;; Effects
		 ((on ?x Ferry)
		  (not (at ?x ?y))
		  (not (empty-ferry)))))))
(setq test3-e '((((PLACE B) (PLACE A) (AT-FERRY A) (ON C1 FERRY) (AT C2 A) (AUTO C1) (AUTO C2)) ((PLACE B) (PLACE A) (AT-FERRY A) (ON C2 FERRY) (AT C1 A) (AUTO C1) (AUTO C2)))))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'successor-states
				       test3-1 test3-2) test3-e)
	   test3-1 test3-2 test3-e)

;;testcases for check-goal
(load "zwen6_src.lisp")
(func-format "check-goal")
(setq test4-1 '(((?x)) ((place a) (place b) (auto c1) (auto c2)
			(at c1 a) (at c2 a) (at-ferry a) (empty-ferry))))
(setq test4-2 (reverse test4-1))
(setq test4-e '(nil nil))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'check-goal
				       test4-1 test4-2) test4-e)
	   test4-1 test4-2 test4-e)
(setq test4-1 '(((location P1) (location P2) (location P3) (location P4)
		(at Monkey P1) (on-floor) (at Box P2) (has-Bananas)
		(at Knife P4))))
(setq test4-2 '(((has-bananas))))
(setq test4-e '(t))
(load "zwen6_src.lisp")
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'check-goal
				       test4-1 test4-2) test4-e)
	   test4-1 test4-2 test4-e)
;;testcases for find-plan
(load "zwen6_src.lisp")
(func-format "find-plan")
(setq test5-1 (list ferry-operators hanoi-operators monkey-operators monkey-operators))
(setq test5-2 (list ferry-inits hanoi-inits monkey-inits-1 monkey-inits-2))
(setq test5-3 (list ferry-goals hanoi-goals monkey-goals-1 monkey-goals-2))
(setq test5-e '(((BOARD C1 A) (SAIL A B) (DEBARK C1 B) (SAIL B A)
		 (BOARD C2 A) (SAIL A B)(DEBARK C2 B))
		((MOVE-DISK D1 D2 P3) (MOVE-DISK D2 D3 P2) (MOVE-DISK D1 P3 D2)
		 (MOVE-DISK D3 P1 P3) (MOVE-DISK D1 D2 P1) (MOVE-DISK D2 P2 D3)
		 (MOVE-DISK D1 P1 D2))
		((GO-TO P2 P1) (PUSH-BOX P4 P2) (GET-KNIFE P4)
		 (PUSH-BOX P3 P4) (CLIMB P3)(GRAB-BANANAS P3))
		((GO-TO P2 P1) (PUSH-BOX P5 P2) (PICK-UP-GLASS P5)
		 (PUSH-BOX P4 P5)(GET-KNIFE P4) (PUSH-BOX P3 P4)
		 (CLIMB P3) (GRAB-BANANAS P3) (GET-WATER P3))))

(mapcar #' result-format3
	   (mapcar #'cpar-rslt (mapcar #'find-plan
				       test5-1 test5-2 test5-3)
		   test5-e)
	   test5-1 test5-2 test5-3 test5-e)
 
