(def nil ())

(def list (lambda* (&rest args) args))

(defmacro lambda (params &rest body)
  (list 'lambda* params (intrinsic/cons 'do body)))

(defmacro defun (name params &rest body)
  (list 'def name
    (intrinsic/cons 'lambda (intrinsic/cons params body))))

(defun + (a b)
  (intrinsic/add a b))

(defun > (a b)
  (intrinsic/greater-than a b))

(defun ! (a)
  (intrinsic/not a))

(defun cons (a b)
  (intrinsic/cons a b))

(defun first (a)
  (intrinsic/first a))

(defun rest (a)
  (intrinsic/rest a))

(defun nil? (a)
  (intrinsic/is-nil a))

(defun sleep (a)
  (intrinsic/sleep a))

(defun eq? (a b)
  (intrinsic/is-eq a b))

(defun list? (a)
  (intrinsic/is-list a))

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
