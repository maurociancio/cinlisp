;autor mauro ciancio

(defun exec (code &optional (input nil) (memory nil) (functions nil))
    (if (null code)
        (run_fun (search_f functions 'main) input (grow_stack memory) functions)
        (cond
            ;parseamos variables globales
            ((eq (caar code) 'int)
            (exec (cdr code) input (add_global_var memory (parse_assignment (car code))) functions))

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
            ;parseamos variables locales
            ((eq (caar f) 'int)
            (run_fun (cdr f) input (add_local_var memory (parse_assignment (car f))) functions output))

            ;scanf
            ((eq (caar f) 'scanf)
            (run_fun (cdr f) (cdr input) (expand_scanf (car input) (cadar f) memory) functions output))

            ;variable = expresion
            ((and (eq (length (car f)) 3) (atom (caar f)) (eq (nth 1 (car f)) '=))
            (run_fun (cdr f) input
                (store_var (parse_assignment (list (caar f) '= (expand (nth 2 (car f)) memory))) memory)
            functions output))

            ;a ++
            ((and (eq (length (car f)) 2) (atom (caar f)) (eq (nth 1 (car f)) '++))
                (run_fun (cons (list (caar f) '= (list (caar f) '+ '1)) (cdr f)) input memory functions output)
            )

            ;printf
            ((eq (caar f) 'printf)
            (run_fun (cdr f) input memory functions (expand_printf (car f) memory output)))
        )
    )
)

;expande la memoria usando el scanf
;i input
;v var
;memory: memoria
(defun expand_scanf (i var memory)
    (store_var (list var i) memory)
)

;expande una llamada a printf
;expr: (printf expresion)
(defun expand_printf (expr &optional (memory nil) (output nil))
    (append output (list (expand (cdr expr) memory)))
)

;expande una expresion
;expr: a, a+b, 3, 3+4, 10+20+4, lala
(defun expand (expr &optional (memory nil))
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
        ((eq (length expr) 1) (expand (car expr) memory))
        ;expresiones booleenas (algo >= algo)
        ((is_in '(!= == > < >= <=) expr) (eval_boolean expr memory))
        ;expresiones numericas
        (t (eval_expr expr memory))
    )
)

;evalua una expresion no booleana
(defun eval_expr (expr memory)
    (if (atom expr)
        (expand expr memory)
        (eval_expr_impl expr memory (inf_pref expr))
    )
)

(defun eval_expr_impl (expr memory prefix)
    (funcall (car prefix) (eval_expr (cadr prefix) memory) (eval_expr (caddr prefix) memory))
)

;evalua una expresion booleana
(defun eval_boolean (expr memory)
    (if (eq (eval_boolean_impl expr memory (inf_pref expr)) t) '1 '0)
)

(defun eval_boolean_impl (expr memory prefix)
    (if (atom expr)
        ;si es un atomo expandimos el valor
        (expand expr memory)
        ;manejamos los casos particulares primero
        (cond
            ((eq (car prefix) '==)
                (eq (eval_expr (cadr prefix) memory) (eval_expr (caddr prefix) memory))
            )
            ((eq (car prefix) '!=)
                (not (eval_boolean_impl expr memory (cons '== (cdr prefix))))
            )
            (t (eval_expr_impl expr memory prefix))
        )
    )
)


;convierte de infijo a prefijo
(defun inf_pref (exp &optional (operadores nil) (operandos nil))
    (cond
        ((atom exp)
            (if (not (null exp))
                exp
                (if (null operadores)
                    (car operandos)
                    (inf_pref exp (cdr operadores) (cons (list (car operadores) (nth 1 operandos) (nth 0 operandos)) (cddr operandos)))
                )
            )
        )
        ((es_operador(car exp))
            (if (null operadores)
                (inf_pref (cdr exp) (list (car exp)) operandos)
                (if (<= (peso (car exp)) (peso (car operadores)))
                    (inf_pref (cdr exp) (cons (car exp) (cdr operadores))
                        (cons (list (car operadores) (nth 1 operandos) (nth 0 operandos)) (cddr operandos))
                    )
                    (inf_pref (cdr exp) (cons (car exp) operadores) operandos)
                )
            )
        )
        (t (inf_pref (cdr exp) operadores (cons(inf_pref(car exp)) operandos)))
    )
)

(defun es_operador (op)
    (contains op '(+ - * / ( ) < > <= >= == !=))
)

;e elemento
;l lista
;retorna si e existe en l
(defun contains (e l)
    (if (null l)
        nil
        (if (equal e (car l))
            t
            (contains e (cdr l))
        )
    )
)

;se fija si alguno de los elementos de any esta en all
(defun is_in (any all)
    (if (null any)
        nil
        (if (contains (car any) all)
            t
            (is_in (cdr any) all)
        )
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

;inicializa la memoria
;( () () )
(defun new_memory ()
    (list (list) (list))
)

;agrega un stack a la memoria
(defun grow_stack (memory)
    (list
        ;global
        (car memory)
        ;local
        (if (null (nth 1 memory))
            (list nil)
            (cons nil (nth 1 memory))
        )
    )
)

;agregar una variable global a la memoria
;memory: ( (globales) (stack) )
; (globales) = ( (a 1) (b 2) (c 3) ... )
; (stack) = ( ( (a 1) (b 2) ) ( (c 4) ))
;var_val  (a 0)
(defun add_global_var (memory var_val)
    ;si la mem es null la inicializamos
    (if (null memory)
        (add_global_var (new_memory) var_val)
        (cons (add_var (car memory) var_val) (cdr memory))
    )
)

;agrega o modifica una variable local
(defun add_local_var (memory var_val)
    (if (null memory)
        ;fallamos por que la memoria no puede ser null
        nil
        (list
            (car memory)
            (cons
                (add_var (caadr memory) var_val)
                (cdadr memory)
            )
        )
    )
)

;agrega o actualiza una variable en memoria
;memory: ( (a 10) (b 20) .... )
;var_val: (a 10)
(defun add_var (memory var_val)
    (if (null memory)
        ;creamos la memoria
        (list var_val)
        ;buscamos si existe
        (if (exists_var (car var_val) memory)
            (replace_var var_val memory)
            (cons var_val memory)
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

;actualiza el valor de una variable en la memoria solo si existe, no la agrega si no existe
;var_val  (a  0)
;memory: ( (a 2) (b 3) (c 4) )
(defun replace_var (var_val memory)
    (if (null memory)
        nil
        (if (eq (caar memory) (car var_val))
            (cons var_val (replace_var var_val (cdr memory)))
            (cons (car memory) (replace_var var_val (cdr memory)))
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

;null fun que no modifica la memoria
(defun null_update (var_var memory)
    memory
)

(defun store_var (var_val memory)
    (if (null memory)
        nil
        ;vemos si hay algun stack
        (if (null (cadr memory))
            ;no hay stacks, solo cambiamos en global
            (store_var_impl var_val memory 'replace_var 'null_update)
            ;hay stacks, vemos si la variable existe en el primer stack, sino cambiamos en el global
            (if (exists_var (car var_val) (caadr memory))
                ;existe la variable en el primer stack, cambiamos esa
                (store_var_impl var_val memory 'null_update 'replace_var)
                ;no existe la variable en el stack, deberia existir en el global, cambiamos global
                (store_var_impl var_val memory 'replace_var 'null_update)
            )
        )
    )
)

;actualiza una variable primero buscando en el stack y luego en el global
;global_update_f: funciones que actualizan la memoria global
;local_update_f: f que actualiza la memoria local (stack)
(defun store_var_impl (var_val memory global_update_f local_update_f)
    (if (null memory)
        nil
        (list
            ;seccion global
            (funcall global_update_f var_val (car memory))
            (if (null (cadr memory))
                nil
                (my_list
                    ;seccion primer stack
                    (funcall local_update_f var_val (caadr memory))
                    (cdadr memory)
                )
            )
        )
    )
)

(defun my_list (e1 e2)
    (cond
        ((and (null e1) (null e2)) '(nil nil))
        ((and (not (null e1)) (null e2)) (list e1))
        ((and (null e1) (not(null e2))) (list e2))
        ((and (not (null e1)) (not(null e2))) (cons e1 e2))
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
        (if (eq (car code) 'int)
            ;declaracion y asignacion
            (if (eq (length code) 2)
                ;caso sin valor inicial
                (list (cadr code) 0)
                ;caso con valor inicial
                (list (cadr code) (nth 3 code))
            )
            ;asignacion
            (list (car code) (nth 2 code))
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
    (trace expand)
    (trace exec)
    (trace run_fun)
    (trace parse_assignment)
    (trace eval_expr)
    (trace eval_expr_impl)
)
;(enable_traces)

(test 'exec-null (exec nil nil) nil)

(test 'parse-assign1 (parse_assignment '(int a)) '(a 0))
(test 'parse-assign2 (parse_assignment '(int b)) '(b 0))
(test 'parse-assign3 (parse_assignment '(int c = 1)) '(c 1))
(test 'parse-assign4 (parse_assignment '(c = 1)) '(c 1))

(test 'add-global-var1 (add_global_var nil '(c 1))
                            '(
                                ((c 1))
                                ()
                            ))

(test 'add-global-var2 (add_global_var '( ((a 2)) () ) '(c 1))
                            '(
                                ((c 1)(a 2))
                                ()
                            ))

(test 'add-global-var3 (add_global_var '( ((a 2)) () ) '(a 10))
                            '(
                                ((a 10))
                                ()
                            ))

(test 'exists-var1 (exists_var 'a nil) nil)
(test 'exists-var2 (exists_var 'a '( (a 1) (b 2) )) t)
(test 'exists-var3 (exists_var 'c '( (a 1) (b 2) )) nil)
(test 'exists-var4 (exists_var 'b '( (a 1) (b 2) )) t)

(test 'replace-var1 (replace_var '(a 10) '( (a 1) (b 2) )) '( (a 10) (b 2) ))
(test 'replace-var2 (replace_var '(c 10) '( (a 1) (b 2) )) '( (a 1) (b 2) ))
(test 'replace-var3 (replace_var '(a 10) '( (a 1) (a 1) )) '( (a 10) (a 10) ))

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

(test 'run-printf3 (exec '
        (
            (int a = 250)
            (main (
                  (printf 50)
                  (printf 150)
                  (printf a)
                  )
            )
        )
    )
    '(50 150 250)
)

(test 'run-printf4 (exec '
        (
            (main (
                  (int a = 250)
                  (int b = 350)
                  (printf a)
                  (printf b)
                  (printf a)
                  )
            )
        )
    )
    '(250 350 250)
)

(test 'run-printf5 (exec '
        (
            (int b = 0)
            (main (
                  (int a = 0)
                  (printf a)
                  (scanf a)
                  (printf a)
                  (printf b)
                  (scanf b)
                  (printf b)
                  )
            )
        )
        ;input
        '(500 250)
    )
    '(0 500 0 250)
)

(test 'run-printf6 (exec '
        (
            (main (
                  (int a = 0)
                  (printf (a + 50))
                  (printf (2 * (a + 50)))
                  )
            )
        )
        ;input
        nil
    )
    '(50 100)
)

(test 'run-printf7 (exec '
        (
            (int b = 5000)
            (main (
                  (int a = 5)
                  (a = 50)
                  (printf a)
                  (a = 10)
                  (printf a)
                  (printf b)
                  )
            )
        )
        ;input
        nil
    )
    '(50 10 5000)
)

(test 'run-printf8 (exec '
        (
            (int b = 5000)
            (main (
                  (int a = 5)
                  (a = 50)
                  (printf a)
                  (a = (a + b))
                  (printf a)
                  (printf b)
                  )
            )
        )
        ;input
        nil
    )
    '(50 5050 5000)
)

(test 'run-printf9 (exec '
        (
            (main (
                  (int a = 5)
                  (a ++)
                  (printf a)
                  )
            )
        )
        ;input
        nil
    )
    '(6)
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

(test 'add_var1 (add_var nil '(a 10)) '((a 10)))
(test 'add_var2 (add_var '((a 50)) '(a 10)) '((a 10)))
(test 'add_var3 (add_var '((a 50)) '(b 10)) '((b 10)(a 50)))

(test 'grow_stack (grow_stack (new_memory)) '(nil (nil)))
(test 'grow_stack2 (grow_stack (grow_stack (new_memory))) '(nil (nil nil)))

(defun test_add_local_var1 ()
    (add_local_var (grow_stack (new_memory)) '(a 10))
)
(test 'add_local_var1 (test_add_local_var1) '(nil (((a 10)))))
(test 'add_local_var2 (add_local_var (test_add_local_var1) '(b 20)) '(nil (((b 20)(a 10)))))
(test 'add_local_var3 (add_local_var (grow_stack (test_add_local_var1)) '(b 20)) '(nil (((b 20))((a 10)))))

(test 'store-var-1 (store_var '(a 10) (new_memory)) '(nil nil))
(test 'store-var-2 (store_var '(a 10) (grow_stack (new_memory))) '(nil (nil nil)))
(test 'store-var-3 (store_var '(a 10) '( ((a 0)) nil)) '( ((a 10)) nil))
(test 'store-var-4 (store_var '(a 10) '( ((a 0)(b 10)) nil)) '( ((a 10)(b 10)) nil))
(test 'store-var-5 (store_var '(a 10) '( ((a 0)) ( ((a 40)) ))) '( ((a 0)) ( ((a 10)) )))
(test 'store-var-6 (store_var '(b 10) '( ((a 0)(b 0)) ( ((a 40)) ))) '( ((a 0)(b 10)) ( ((a 40)) )))
(test 'store-var-7 (store_var '(a 10) '( ((a 0)) ( ((a 0)) ((a 0)) ))) '( ((a 0)) ( ((a 10)) ((a 0)) )))
(test 'store-var-8 (store_var '(a 10) '( ((a 0)) ( ((a 0)) ((a 0))((a 0)) ))) '( ((a 0)) ( ((a 10)) ((a 0)) ((a 0)) )))

(test 'is_in1 (is_in '(1 2) '(1 2 3)) t)
(test 'is_in2 (is_in '(2) '(1 2 3)) t)
(test 'is_in3 (is_in 'nil '(1 2 3)) nil)
(test 'is_in4 (is_in '(1) '(2 3)) nil)

(test 'inf2pre (inf_pref '(1 + 2)) '(+ 1 2))
(test 'inf2pre2 (inf_pref '((1 + 2))) '(+ 1 2))
(test 'inf2pre3 (inf_pref '((1 + 2) * 2)) '(* (+ 1 2) 2))

(test 'expand1 (expand '((1 + 2) * 2)) 6)
(test 'expand2 (expand '(2 * 2)) 4)
(test 'expand3 (expand '(10 / 2)) 5)
(test 'expand4 (expand '(a) '( ((a 5)) nil)) 5)
(test 'expand5 (expand 'a '( ((a 5)) nil)) 5)
(test 'expand6 (eval_expr 'a '( ((a 5)) nil)) 5)
(test 'expand7 (expand '(a + 50) '( ((a 5)) nil)) 55)
(test 'expand8 (expand '(a + a) '( ((a 5)) nil)) 10)

(test 'bool1 (eval_boolean '(1 == 1) nil) 1)
(test 'bool2 (eval_boolean '((1+2) == 0) nil) 0)
(test 'bool3 (eval_boolean '(1 == 0) nil) 0)
(test 'bool4 (eval_boolean '((1 + 2) == 3) nil) 1)
(test 'bool5 (eval_boolean '((1 + 2) != 3) nil) 0)
(test 'bool6 (eval_boolean '((1 + 2) != 4) nil) 1)
(test 'bool7 (eval_boolean '((1 + 2) >= 4) nil) 0)
(test 'bool7 (eval_boolean '((1 + 2) < 4) nil) 1)
