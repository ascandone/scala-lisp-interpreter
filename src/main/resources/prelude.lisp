(def nil ())

(def list (lambda* (&rest args) args))

(defmacro lambda (params &rest body)
  (list 'lambda* params (builtin/cons 'do body)))

(defmacro defun (name params &rest body)
  (list 'def name
    (builtin/cons 'lambda (builtin/cons params body))))

(defun + (a b)
  (builtin/add a b))

(defun > (a b)
  (builtin/greater-than a b))

(defun ! (a)
  (builtin/not a))

(defun cons (a b)
  (builtin/cons a b))

(defun first (a)
  (builtin/first a))

(defun rest (a)
  (builtin/rest a))

(defun nil? (a)
  (builtin/is-nil a))

(defun sleep (a)
  (builtin/sleep a))

(defun receive () (builtin/receive))
(defun send (a b) (builtin/send a b))
(defun self () (builtin/self))

(defun log (a)
  (builtin/log a))

(defun eq? (a b)
  (builtin/is-eq a b))

(defun list? (a)
  (builtin/is-list a))

(defun second (lst)
  (first (rest lst)))

(defun foldr (lst z f)
  (if (nil? lst)
    z
    (f
      (first lst)
      (foldr (rest lst) z f))))

(defun concat (&rest nested)
  (foldr
    nested
    ()
    (lambda (xs ys) (foldr xs ys cons))))

(defun mapcat (lst f)
  (foldr lst ()
    (lambda (el acc) (concat (f el) acc))))

(defun map (lst f)
  (foldr lst ()
    (lambda (el acc) (cons (f el) acc))))

(defun backquote-helper (nested)
  (if (list? nested)
    (if (eq? (first nested) 'unquote)
      (list 'list (second nested))
      (if (eq? (first nested) 'unquote-splicing)
        (second nested)
        (list 'list (list 'backquote nested))))
    (list 'list (list 'quote nested))))

(defmacro backquote (body)
  (if (list? body)
    (if (eq? (first body) 'unquote)
      (second body)
      (cons 'concat (map body backquote-helper)))
    (list 'quote body)))

(defmacro let1 (binding body)
    `((lambda (,(first binding)) ,body)
        ,(second binding)))

(defmacro let (pairs body)
  (if (nil? pairs)
    body
    `(let1 (,@(first pairs))
        (let (,@(rest pairs)) ,body))))


(defmacro fork (&rest body)
  `(builtin/fork (lambda () ,@body)))
