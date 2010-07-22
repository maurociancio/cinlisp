;autor mauro ciancio

(defun exec (code input &optional (memory nil))
	nil
)

;=============================
;testing function
;=============================
(defun test (name got expected)
	(if (equal expected got)
		;t
		(progn (print '==ok==) (print name))
		(progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
	)
)

;=============================
;tests
;=============================
(test 'exec-null (exec nil nil) nil)
