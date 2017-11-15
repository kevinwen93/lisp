;;; Test cases for grading Lisp 1: 9/15/2016
;;; Gene Kim.
;;;
;;; Problems
;;;     unifier
;;;     apply-unifier
;;;     extract-constants
;;;     prune-unifiers
;;;

(load "test_framework.lisp")


;;;;;;;;;;;;;;;;;;
;;; unifier
;;;;;;;;;;;;;;;;;;

;; Given examples.
(create-test 'unifier '((a b c) (a b c)) t)
(create-test 'unifier '((?x b ?y) (a b c)) '((?x a) (?y c)))
(create-test 'unifier '((?x ?x ?y) (a a c)) '((?x a) (?y c)))
(create-test 'unifier '((?x ?x ?y) (a b c)) nil)

;; Evaluates to T.
(create-test 'unifier '(() ()) T)
(create-test 'unifier '((a) (a)) T)
(create-test 'unifier '((a b) (a b)) T)
(create-test 'unifier '((a b c d) (a b c d)) T)

;; Evaluates to unifier.
(create-test 'unifier '((?x) (a)) '((?x a)))
(create-test 'unifier '((a b c ?x) (a b c d)) '((?x d)))
(create-test 'unifier '((?x ?y) (a a)) '((?x a) (?y a)))
(create-test 'unifier '((?x ?f ?eqw ?e ?b ?d) (a b c d s e)) '((?x a) (?f b) (?eqw c) (?e d) (?b s) (?d e)))
(create-test 'unifier '((?x ?f ?eqw ?e ?b ?d) (a a a a a a)) '((?x a) (?f a) (?eqw a) (?e a) (?b a) (?d a)))
(create-test 'unifier '((?x ?x ?x ?x ?x) (a a a a a)) '((?x a)))
(create-test 'unifier '((?x ?x ?x a ?x) (a a a a a)) '((?x a)))

;; Unsatisfied.
(create-test 'unifier '((?x ?x) (a b)) nil)
(create-test 'unifier '((?x a ?x) (a a b)) nil)
(create-test 'unifier '((a b c) (a b d)) nil)
(create-test 'unifier '((?x b ?y) (a a b)) nil)
(create-test 'unifier '((?x ?x ?x ?x a) (a a a b a)) nil)
(create-test 'unifier '((?x ?x ?x ?x a) (a a a a b)) nil)

;; Unequal lengths.
(create-test 'unifier '(nil (a)) nil)
(create-test 'unifier '((a) nil) nil)
(create-test 'unifier '((a) (a b)) nil)
(create-test 'unifier '((a b) (a)) nil)

;; Invalid input.
(create-test 'unifier '((a b c) (?x a b)) nil)

;; Strange input.
(create-invalid-input-test 'unifier '("a" (a b c)) nil)
(create-invalid-input-test 'unifier '(a (a b c)) nil)
(create-invalid-input-test 'unifier '(1 (a b c)) nil)
(create-invalid-input-test 'unifier '(t (a b c)) nil)
(create-invalid-input-test 'unifier '((a b c) "a") nil)
(create-invalid-input-test 'unifier '((a b c) a) nil)
(create-invalid-input-test 'unifier '((a b c) 1) nil)
(create-invalid-input-test 'unifier '((a b c) t) nil)


;;;;;;;;;;;;;;;;;;
;;; apply-unifier
;;;;;;;;;;;;;;;;;;

;; Given examples
(create-test 'apply-unifier '(((?x a) (?y b)) (on ?x ?y)) '(on a b))
(create-test 'apply-unifier '(((?x a)) (on ?x ?y)) '(on a ?y))
(create-test 'apply-unifier '(((?x a) (?y b) (?z c)) (on ?x ?y)) '(on a b))
(create-test 'apply-unifier '(((?x a) (?x b)) (on ?u ?v)) '(on ?u ?v))

;; Swap order
(create-test 'apply-unifier '(((?x a) (?y b)) (on ?y ?x)) '(on b a))

;; Repeated
(create-test 'apply-unifier '(((?x a) (?y b)) (on ?x ?x)) '(on a a))

;; Edge Cases
(create-test 'apply-unifier '(() (on ?u ?v)) '(on ?u ?v))
(create-test 'apply-unifier '(() (p)) '(p))
(create-test 'apply-unifier '(((?x a) (?y b)) (p)) '(p))

;; Big examples
(create-test 'apply-unifier '(((?x a) (?y b) (?a w) (?w c) (?p q)) (bigp ?p ?a ?y ?w)) '(bigp q w b c))

;; Invalid input
(create-invalid-input-test 'apply-unifier '(() ()) nil)
(create-invalid-input-test 'apply-unifier '(() a) nil)
(create-invalid-input-test 'apply-unifier '(() "a") nil)
(create-invalid-input-test 'apply-unifier '(((a b)) (on a b)) nil)
(create-invalid-input-test 'apply-unifier '(a (on a b)) nil)
(create-invalid-input-test 'apply-unifier '("a" (on a b)) nil)


;;;;;;;;;;;;;;;;;;
;;; extract-constants
;;;;;;;;;;;;;;;;;;

;; Simple cases.
(create-test 'extract-constants '(((on table a) (on table b) (on c b) (clear a) (clear c))) '(on table a b c clear))
(create-test 'extract-constants '(((on table c) (on a a) (on b c) (clear b))) '(on table clear a b c))
(create-test 'extract-constants '(((P a b))) '(p a b))
(create-test 'extract-constants '(((P a b c d e))) '(p a b c d e))
(create-test 'extract-constants '(((P a a a a a))) '(p a))

;; Include variables
(create-test 'extract-constants '(((on ?table ?a) (on ?table ?b) (on ?c ?b) (clear ?a) (clear ?c))) '(on clear))
(create-test 'extract-constants '(((on ?x a))) '(on a))

;; Include negative literals
(create-test 'extract-constants '(((not (on a b)))) '(on a b))
(create-test 'extract-constants '(((not (on a b)) (on a b))) '(on a b))
(create-test 'extract-constants '(((not (on a b)) (clear x))) '(on clear a b x))

;; Edge cases                                    
(create-test 'extract-constants '(()) nil)

;; Bad input
(create-invalid-input-test 'extract-constants '(a) nil)
(create-invalid-input-test 'extract-constants '("a") nil)
(create-invalid-input-test 'extract-constants '(((not (not (on a))))) nil)
(create-invalid-input-test 'extract-constants '(((on a b) (not (clear x)) (on a (clear x) c))) nil)
(create-invalid-input-test 'extract-constants '(((on a b) (clear (clear x)))) nil)


;;;;;;;;;;;;;;;;;;
;;; prune-unifiers
;;;;;;;;;;;;;;;;;;

(setq u1 '((?x a) (?y h) (?z q)))
(setq u2 '((?x c) (?m l)))
(setq u3 '())
(setq u4 '((?l a)))

(setq d1 '((?x a) (?y a)))
(setq d2 '((?l g) (?m a) (?n w) (?e a)))

;; No prunes
(create-test 'prune-unifiers (list (list u1)) (list u1))
(create-test 'prune-unifiers (list (list u1 u2)) (list u1 u2))
(create-test 'prune-unifiers (list (list u1 u3 u2)) (list u1 u3 u2))
(create-test 'prune-unifiers (list (list u3)) (list u3))
(create-test 'prune-unifiers (list '()) '())
(create-test 'prune-unifiers (list (list u4)) (list u4))
(create-test 'prune-unifiers (list (list u4 u3 u2 u1)) (list u4 u3 u2 u1))

;; Pruning necessary
(create-test 'prune-unifiers (list (list d1)) '())
(create-test 'prune-unifiers (list (list d1 d2)) '())
(create-test 'prune-unifiers (list (list d1 u4)) (list u4))
(create-test 'prune-unifiers (list (list d2 u2)) (list u2))
(create-test 'prune-unifiers (list (list u4 d1 u3 u2 u1 d2)) (list u4 u3 u2 u1))

;; Bad input.
(create-invalid-input-test 'prune-unifiers (list "a") nil)
(create-invalid-input-test 'prune-unifiers (list 1) nil)
(create-invalid-input-test 'prune-unifiers (list (list 1 2 3)) nil) 
(create-invalid-input-test 'prune-unifiers (list (list "a" '())) nil) 
(create-invalid-input-test 'prune-unifiers (list (list "a" u1)) nil) 
(create-invalid-input-test 'prune-unifiers (list (list u1 1)) nil) 
(create-invalid-input-test 'prune-unifiers (list (list u1 '((?x a) (?y b b)))) nil) 


(defun unifier-equal (x1 x2)
  (or (and (eq x1 t) (eq x2 t))
      (set-and-length-equal x1 x2)))

;;; Runs all test with appropriate equality checkers.
(defun run-all-normal-tests ()
  (format t "~S~%" (test 'unifier #'unifier-equal))
  (format t "~S~%" (test 'apply-unifier #'set-and-length-equal))
  (format t "~S~%" (test 'extract-constants #'set-and-length-equal))
  (format t "~S~%" (test 'prune-unifiers #'set-and-length-equal)))

(defun run-all-invalid-input-tests ()
  (format t "~S~%" (test 'unifier #'unifier-equal t))
  (format t "~S~%" (test 'apply-unifier #'set-and-length-equal t))
  (format t "~S~%" (test 'extract-constants #'set-and-length-equal t))
  (format t "~S~%" (test 'prune-unifiers #'set-and-length-equal t)))
