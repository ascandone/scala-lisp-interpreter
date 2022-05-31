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

(defun second (lst)
  (first (rest lst)))
