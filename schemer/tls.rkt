(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     (else (and (atom? (car l)) (lat? (cdr l)))))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define rmember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rmember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (cdr lat))))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new old (cdr lat)))))))

(define multirmember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirmember a (cdr lat)))
     (else (cons (car lat) (multirmember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define +:
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (+: (add1 n) (sub1 m))))))

(define -:
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (-: (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+: (car tup) (addtup (cdr tup)))))))

(define *:
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+: n (*: n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+: (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >:
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (>: (sub1 n) (sub1 m))))))

(define <:
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (<: (sub1 n) (sub1 m))))))

(define =:
  (lambda (n m)
    (and (not (>: n m)) (not (<: n m)))))


(define ^:
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (*: n (^: n (sub1 m)))))))

(define quot
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quot (- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((=: n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((=: n 1) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (n m)
    (cond
     ((and (number? n) (number? m)) (=: n m))
     ((or (number? n) (number? m)) #f)
     (else (eq? n m)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (=: n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eq? (car l) a)) (rember* a (cdr l)))
     ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((and (atom? (car lat)) (eq? (car lat) old))
      (cons (car lat) (cons new (insertR* new old (cdr lat)))))
     ((atom? (car lat)) (cons (car lat) (insertR* new old (cdr lat))))
     (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((and (atom? (car l)) (eq? a (car l))) (add1 (occur* a (cdr l))))
     ((atom? (car l)) (occur* a (cdr l)))
     (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eq? (car l) old))
      (cons new (subst* new old (cdr l))))
     ((atom? (car l)) (cons (car l) (subst* new old (cdr l))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eq? (car l) old))
      (cons new (cons old (insertL* new old (cdr l)))))
     ((atom? (car l)) (cons (car l) (insertL* new old (cdr l))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((and (atom? (car l)) (eq? (car l) a)) #t)
     ((atom? (car l)) (member*? a (cdr l)))
     (else (or (member*? a (car l)) (member*? a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

;; mutually recursive fns

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define rember
  (lambda (a l)
    (cond
     ((null? l) '())
     ((equal? (car l) a) (cdr l))
     (else (cons (car l) (rember a (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp)) (numbered? (caddr aexp)))))))

(define operator
  (lambda (aexp)
    (cadr aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (+: (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
      (*: (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     (else (^: (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define +::
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (+:: (edd1 n) (zub1 m))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((set? lat) lat)
     (else (cons (car lat) (makeset (multirmember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (makeset (append set1 set2))))

(define set-difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (set-difference (cdr set1) set2))
     (else (cons (car set1) (set-difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersectall (cons (intersect (car l-set) (cadr l-set)) (cddr l-set)))))))

(define pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cddr x)) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (second (car rel)) (seconds (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) (rember-f test? a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons new (cons old (cdr lat))))
       (else (cons (car lat) (insertL-f new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons (car lat) (cons new (cdr lat))))
       (else (cons (car lat) (insertR-f new old (cdr lat))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((eq? (car lat) old) (seq new old lat))
       (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insertR
  (insert-g (lambda (new old lat) (cons (car lat) (cons new (cdr lat))))))

(define insertL
  (insert-g (lambda (new old lat) (cons new (cons old (cdr lat))))))

(define subst
  (insert-g (lambda (new old lat) (cons new (cdr lat)))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? '+ x) +:)
     ((eq? '* x) *:)
     (else ^:))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat) L R)))))))


(multiinsertLR&co 'salty 'fish 'chips '(newlat and fish or fish and chips)
                  (lambda (newlat L R) newlat))

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (even? (car l))) (cons (car l) (evens-only* (cdr l))))
     ((not (atom? (car l))) (cons (evens-only* (car l)) (evens-only* (cdr l))))
     (else (evens-only* (cdr l))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() '()))
     ((and (atom? (car l)) (even? (car l)))
      (evens-only*&co (cdr l) (lambda (evens odds)
                                (col (cons (car l) evens) odds))))
     ((not (atom? (car l)))
      (cons (evens-only*&co (car l) (lambda (evens odds) (list evens odds)))
            (evens-only*&co (cdr l) (lambda (evens odds) (list evens odds)))))
     (else (evens-only*&co (cdr l) (lambda (evens odds)
                                     (col evens (cons (car l) odds))))))))
