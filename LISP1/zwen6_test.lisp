(defun result-format1 (rslt input crrc_rslt)
  (format t "~:[FAIL~;pass~] ... Input: ~d Expected: ~d~%"
	  rslt input crrc_rslt))

(defun result-format2 (rslt input1 input2 crrc_rslt)
  (format t "~:[FAIL~;pass~] ... Input: ~d and ~d Expected: ~d~%"
	  rslt input1 input2  crrc_rslt))

(defun func-formart (funcname)
  (format t "~%Runing test for ~d~%" funcname))
  
(defun cpar-rslt (func_rslt  crrc_rslt)
  (equal func_rslt crrc_rslt))

(load "zwen6_src.lisp")

;;testcase for unifier function
(func-formart "unifier")

(setq tset1-1 '(a (a b c) (a b c) (?x b ?y) (?x ?x ?y) (?x ?x ?y)))
(setq tset1-2 '((a b c) a (a b c) (a b c) (a a c) (a b c)))
(setq tset1-e '(nil nil t ((?y c) (?x a)) ((?y c) (?x a)) nil))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'unifier tset1-1 tset1-2) tset1-e)
	   tset1-1 tset1-2 tset1-e)

;;testcase for apply-unifier function
(func-formart "apply-unifier")

(setq ucset '(a (a (?x a)) ((?x a) (?y a b)) ((a a)) ((?x ?y)) ((?x a) (?x a))))
(setq ucset-r '((on ?x ?y) (on ?x ?y)
		(on ?x ?y) (on ?x ?y) (on ?x ?y) (on ?x ?y)))
(setq nset '(nil nil nil nil nil nil nil nil nil nil))

(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'apply-unifier ucset ucset-r) nset)
	   ucset ucset-r nset)
(setq tset2-1 '(((?x a)) ((?x a) (?y b)) ((?x a)) ((?x a) (?y b) (?z c))
		((?x a) (?y b))))
(setq tset2-2 '(a (on ?x ?y) (on ?x ?y) (on ?x ?y) (on ?u ?v)))
(setq tset2-e '(nil (on a b) (on a ?y) (on a b) (on ?u ?v)))
(mapcar #' result-format2
	   (mapcar #'cpar-rslt (mapcar #'apply-unifier tset2-1 tset2-2) tset2-e)
	   tset2-1 tset2-2 tset2-e)

;;testcase for extract-constants
(func-formart "extract-constants")

(setq lcset '(a (a (on a b)) ((a) (on a b)) ((on (a b))) ((on a b) (?x b c))
	      ((not a)) ((not)) ((not (a))) ((not (?x a))) ((not (on (a b))))))
(mapcar #' result-format1
	   (mapcar #'cpar-rslt (mapcar #'extract-constants lcset) nset)
	   lcset  nset)
(setq tset3 '((clear ?x) (clear ?y) (not (sleeping robbie))
	      (empty-handed robbie) (at robbie table)))
(setq tset3-c '(clear sleeping empty-handed at robbie table))
(result-format1 (cpar-rslt (extract-constants tset3) tset3-c) tset3 tset3-c)

;;testcase for extract-constants
(func-formart "prune-unifiers")
(setq ucsets '((a) ((a (?x a))) (((?x a) (?y a b))) (((a a)))
	       (((?x ?y))) (((?x a) (?x a)))))
(mapcar #' result-format1
	   (mapcar #'cpar-rslt (mapcar #'prune-unifiers ucsets) nset)
	   ucsets nset)
(setq tset4 '(a (((?x a) (?y b)) ((?x a) (?y a) (?z b)) ((?u c) (?v x)))))
(setq tset4-c'(nil (((?x a) (?y b)) ((?u c) (?v x)))))
(mapcar #' result-format1
	   (mapcar #'cpar-rslt (mapcar #'prune-unifiers tset4) tset4-c)
	   tset4 tset4-c)
