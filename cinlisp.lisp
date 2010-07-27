;autor mauro ciancio

(defun exec (code &optional (input nil) (memory nil) (functions nil))
    (if (null code)
        (run_fun (search_f functions 'main) input memory functions)
        (cond
            ;parseamos variables globales
            ((eq (caar code) 'int)
            (exec (cdr code) input (add_global_var memory (car code)) functions))

            ;parseamos las funciones
            (t (exec (cdr code) input memory (add_function functions (car code))))
        )
    )
)

;corre la funcion f
;input: entrada (valor valor2 ....)
;memory: memoria
;functions: funciones declaradas
;output: salida (a b c d ...)
(defun run_fun (f &optional (input nil) (memory nil) (functions nil) (output nil))
    (if (null f)
        output
        (cond
            ;printf
            ((eq (caar f) 'printf)
            (run_fun (cdr f) input memory functions (expand_printf (car f) input memory functions output)))
        )
    )
)

;expande una llamada a printf
;expr: (printf expresion)
(defun expand_printf (expr &optional (input nil) (memory nil) (functions nil) (output nil))
    (append output (list (expand (cdr expr) input memory functions output)))
)

;expande una expresion
;expr: a, a+b, 3, 3+4, 10+20+4, lala
(defun expand (expr &optional (input nil) (memory nil) (functions nil) (output nil))
    (cond
            ;expresion nula
            ((null expr) nil)
            ;es un atomo
            ((atom expr)
                (if (numberp expr)
                    ;es un numero literal
                    expr
                    ;es una variable, hay que buscarla en la memoria
                    (search_var expr memory)
                )
            )
            ;es una lista con un atomo
            ((eq (length expr) 1) (expand (car expr) input memory functions output))
            (t expr)
    )
)

;busca en las funciones la funcion con nombre name y la devuelve
;funciones: (f1 f2 ... f3)
;devuelve f que se llame name
(defun search_f (functions name)
    (if (null functions)
        nil
        (if (eq (caar functions) name)
            (cadar functions)
            (search_f (cdr functions) name)
        )
    )
)

;agregar una variable global a la memoria
;memory: ( (globales) (stack) )
; (globales) = ( (a 1) (b 2) (c 3) ... )
; (stack) = ( ( (a 1) (b 2) ) ( (c 4) ))
;code:  int a = 0
;       int b
(defun add_global_var (memory code)
    ;si la mem es null la inicializamos
    (if (null memory)
        (list (list (parse_assignment code)) (list))
        (if (exists_var (cadr code) (car memory))
            (cons (replace_var code (car memory)) (cdr memory))
            (cons (cons (parse_assignment code) (car memory)) (cdr memory))
        )
    )
)

;verifica que var este en la memoria
;memory: ( (a 1) (b 2) (c 3) )
;var: a
(defun exists_var (var memory)
    (if (null memory)
        nil
        (if (eq (caar memory) var)
            t
            (exists_var var (cdr memory))
        )
    )
)

;code:  int a = 0
;       int b
;memory: ( (a 2) (b 3) (c 4) )
(defun replace_var (code memory)
    (if (null memory)
        nil
        (if (eq (caar memory) (cadr code))
            (cons (parse_assignment code) (replace_var code (cdr memory)))
            (cons (car memory) (replace_var code (cdr memory)))
        )
    )
)

;busca una variable primero en el stack y luego en el sector global
(defun search_var (var memory)
    (if (null memory)
        nil
        ;busco primero en el primer stack
        (if (exists_var var (caadr memory))
            (get_value var (caadr memory))
            ;luego en el sector global
            (get_value var (car memory))
        )
    )
)

;busca el valor de una variable en la memoria
;var: a
;memory: ( (a 20) .... )
(defun get_value (var memory)
    (if (null memory)
        nil
        (if (eq (caar memory) var)
            (cadar memory)
            (get_value var (cdr memory))
        )
    )
)

;code:  int a = 0
;       int b
;devuelve (a 0)
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

;functions: lista de funciones declaradas ( f f2 f3 ... )
;f: nueva funcion a agregar
(defun add_function (functions f)
    (cons f functions)
)

;=============================
;testing function
;=============================
(defun test (name got expected)
    (if (equal expected got)
        t
        ;(progn (print '==ok==) (print name))
        (progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
    )
)

;=============================
;tests
;=============================
(defun enable_traces ()
    (trace exec)
    (trace run_fun)
)
;(enable_traces)

(test 'exec-null (exec nil nil) nil)

(test 'parse-assign1 (parse_assignment '(int a)) '(a 0))
(test 'parse-assign2 (parse_assignment '(int b)) '(b 0))
(test 'parse-assign3 (parse_assignment '(int c = 1)) '(c 1))

(test 'add-global-var1 (add_global_var nil '(int c = 1)) 
                            '(
                                ((c 1))
                                ()
                            ))

(test 'add-global-var2 (add_global_var '( ((a 2)) () ) '(int c = 1)) 
                            '(
                                ((c 1)(a 2))
                                ()
                            ))

(test 'exists-var1 (exists_var 'a nil) nil)
(test 'exists-var2 (exists_var 'a '( (a 1) (b 2) )) t)
(test 'exists-var3 (exists_var 'c '( (a 1) (b 2) )) nil)
(test 'exists-var4 (exists_var 'b '( (a 1) (b 2) )) t)

(test 'replace-var1 (replace_var '(int a = 10) '( (a 1) (b 2) )) '( (a 10) (b 2) ))
(test 'replace-var2 (replace_var '(int c = 10) '( (a 1) (b 2) )) '( (a 1) (b 2) ))
(test 'replace-var3 (replace_var '(int a = 10) '( (a 1) (a 1) )) '( (a 10) (a 10) ))

(test 'add-function (add_function nil '(main ())) '((main () )) )

(test 'search_f (search_f '( (main (aaaa)) (p () ) ) 'main) '(aaaa))
(test 'search_f2 (search_f '( (main (aaaa)) (p () ) ) 'p) '())

(test 'run-empty-main (exec '
        (
            (main
            )
        )
    )
    nil
)

(test 'run-printf (exec '
        (
            (main (
                  (printf 50)
                  )
            )
        )
    )
    '(50)
)

(test 'run-printf2 (exec '
        (
            (main (
                  (printf 50)
                  (printf 150)
                  )
            )
        )
    )
    '(50 150)
)


(test 'expand1 (expand '2) '2)
(test 'expand2 (expand nil) nil)
(test 'expand3 (expand '(2)) '2)

(test 'expand-printf (expand_printf '(printf 2)) '(2))
(test 'expand-printf2 (expand_printf '(printf (2))) '(2))

(test 'search_var1 (search_var 'a '( ((a 10)) () ) ) '10)
(test 'search_var2 (search_var 'b '( ((a 10)(b 50)) () ) ) '50)
(test 'search_var3 (search_var 'b '( ((a 10)(b 50)) (((b 1000)) ( (b 500)) ) ) ) '1000)
(test 'search_var4 (search_var 'b '( ((a 10)(b 50)) (((b 1000)) ) ) ) '1000)
(test 'search_var5 (search_var 'a '( ((a 10)(b 50)) (((b 1000)) ) ) ) '10)
