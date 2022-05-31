(def nil ())

(def list (lambda* (&rest args) args))

(defmacro lambda (params &rest body)
  (list 'lambda* params (cons 'do body)))