;autor mauro ciancio

;testing function
;=============================
(defun test (name got expected)
	(if (equal expected got)
		;t
		(progn (print '==ok==) (print name))
		(progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
	)
)
