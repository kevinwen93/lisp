

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

(func-formart "count-occur")
(result-format2(cpar-rslt (count-occur 'x '(x x a b)) 2)
	       'x '(x x a b) 2)
(result-format2(cpar-rslt (count-occur 'x '((f x) y (((x z) () x)))) 3)
	       'x '((f x) y (((x z) () x))) 3)

(func-formart "subexpr")
(result-format2(cpar-rslt (subexpr 'a 'a) t)
	       'a 'a t)
(result-format2(cpar-rslt (subexpr '(a "and" #\b) '(A "and" #\b) ) t)
	       '(a "and" #\b) '(A "and" #\b) t)
(result-format2(cpar-rslt (subexpr '(a b c) '(d (a b c) (e f g))) 
			  '((a b c) (e f g)))
	       '(a b c) '(d (a b c) (e f g))  '((a b c) (e f g)))
(result-format2(cpar-rslt (subexpr 'b '(d (a b c)) ) 
			  '(b c))
	       'b '(d (a b c))  '(b c))
(result-format2(cpar-rslt (subexpr 'b '(d (a b c)) ) 
			  '(b c))
	       'b '(d (a b c))  '(b c))
(result-format2(cpar-rslt (subexpr '(a b c) '(d a b c) ) 
			  nil)
	       '(a b c) '(d a b c)  nil)
