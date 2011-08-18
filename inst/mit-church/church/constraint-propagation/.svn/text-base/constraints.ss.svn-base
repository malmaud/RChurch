#!r6rs

(library (church constraint-propagation constraints)

         (export *wildcard-value*
                 wildcard?
                 make-constraint-set
                 constraint-set?
                 constraint-set-contains?
                 wildcard-constraint-set?
                 constraint-set-union
                 *wildcard-constraint-set*
                 constraint-violated?
                 constraint-set->values)

         (import (_srfi :1)
                 (church utils rnrs)
                 (church readable-scheme))

         (define *wildcard-value* 'value-wildcard)

         (define (wildcard? x) (eq? x *wildcard-value*))


         (define (make-constraint-set constraints)
           (list 'constraint-set constraints))
         
         (define (constraint-set? s)
           (tagged-list? s 'constraint-set))

         (define (constraint-set-contains? s val)
           (any (lambda (x) (equal? x val))
                (constraint-set->values s)))

         (define (wildcard-constraint-set? s)
           (constraint-set-contains? s *wildcard-value*))

         (define (constraint-set-union constraint-sets)
           (make-constraint-set (apply lset-union (pair equal? (map constraint-set->values constraint-sets)))))

         (define *wildcard-constraint-set*
           (make-constraint-set (list *wildcard-value*)))

         (define (constraint-violated? val constraint-set)
           (and
            (not (wildcard-constraint-set? constraint-set))
            (not (constraint-set-contains? constraint-set val))))

         (define constraint-set->values second)
         
)