(define _true (lambda (then else) then))

(define _false (lambda (then else) else))

;; (define _if (lambda (condition then else) (condition then else)))

(define-syntax-rule (_if condition then else)
  ((condition (lambda () then) (lambda () else))))

(define _not (lambda (condition) (_if condition _false _true)))

(define _pair
  (lambda (l r)
    (lambda (selector)
      (selector l r))))

(define _lhs
  (lambda (pair)
    (pair (lambda (l r) l))))

(define _rhs
  (lambda (pair)
    (pair (lambda (l r) r))))

(_lhs (_pair 'l 'r))
(_rhs (_pair 'l 'r))

(define _nil
  (lambda (selector) (selector _true)))

(define _null?
  (lambda (pair)
    (pair (pair (lambda (l r) _false)))))

(_null? _nil)
