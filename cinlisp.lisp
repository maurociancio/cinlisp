;autor mauro ciancio

(defun exec (code input &optional (memory nil))
	(if (null code)
		nil
		(cond
		;parseamos variables globales
			(eq (caar code) 'int)
			(exec (cdr code) input (add_global_var memory (car code)))

		;main
			(t nil)
		)
	)
)

(defun exec_main (code input &optional (memory nil))
)

;agregar una variable global a la memoria
;memory: ( (globales) (stack) )
; (globales) = ( (a 1) (b 2) (c 3) ... )
; (stack) = ( ( (a 1) (b 2) ) ( (c 4) ))
;code: 	int a = 0
;	int b
(defun add_global_var (memory code)
	;si la mem es null la inicializamos
	(if (null memory)
		(list (list) (list))
	)
)

;code: 	int a = 0
;	int b
(defun parse_assignment (code)
	(if (null code)
		nil
		(if (eq (length code) 2)
			;caso sin valor inicial
			(list (cadr code) 0)
			;caso con valor inicial
			(list (cadr code) (nth 3 code))
		)
	)
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

(test 'parse-assign1 (parse_assignment '(int a)) '(a 0))
(test 'parse-assign2 (parse_assignment '(int b)) '(b 0))
(test 'parse-assign3 (parse_assignment '(int c = 1)) '(c 1))
