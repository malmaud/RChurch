#!r6rs

;;
;; PLT FFI bindings for various parts of GSL that church needs:
;;   random number generators,
;;   continuous and discrete PDFs and random number samplers,
;;   factorial functions
;;
;; Typical usage:
;;   (gsl-bernoulli 0.5)
;;   (gsl-beta-pdf a b (gsl-beta a b))
;;
;; All samplers accept an optional stateful random number generator
;; (RNG).  The default is the taus RNG, which is fast.
;;
;;   (define rng (gsl-rng-alloc gsl-rngt-taus))
;;   (gsl-bernoulli 0.5 rng)
;;   (gsl-rng-free rng)
;;
;; Functions convert integers / doubles to the needed types automatically.
;;
;; We provide bindings for bernoulli, beta, exponential, gamma, poisson,
;; gaussian, multinomial, and dirichlet distributions.
;;
;; GSL supports more: cauchy, chisq, dirichlet, erlang, fdist, landau,
;; geometric, hypergeometric, gumbel, logistic, negative binomial,
;; pascal, pareto, rayleigh, tdist, laplace, weibull, levy, and more...
;;
;; They would be easy to add; I just haven't done it.
;;
;; D. Wingate 1/28/09
;;

(library (church external gsl-bindings)
         
         (export gsl-lnfact gsl-lngamma
		 gsl-rng-set gsl-rng-randomize
		 gsl-rng-uniform-int gsl-rng-uniform-pos
                 gsl-bernoulli gsl-bernoulli-pdf gsl-beta gsl-beta-pdf gsl-exponential gsl-exponential-pdf
                 gsl-gamma gsl-gamma-pdf gsl-poisson gsl-poisson-pdf gsl-gaussian gsl-gaussian-pdf
                 gsl-tdist gsl-tdist-pdf
                 gsl-multinomial gsl-multinomial-pdf gsl-multinomial-lnpdf
                 gsl-dirichlet gsl-dirichlet-pdf gsl-dirichlet-lnpdf
                 gsl-discrete gsl-discrete-pdf)
         
         (import (_srfi :1)
		 (only (_srfi :19) current-time time-nanosecond)
                 (mzlib foreign)
                 (only (scheme mpair) mlist->list list->mlist mlist?)
                 (only (mzscheme) inexact->exact exact->inexact)
                 (church external gsl-paths)
                 (church utils rnrs))
         
         (unsafe!)
         
         
         ;; =============================================================
         ;;
         ;; Library paths
         ;;

         (define (safe-ffi-lib lib-path)
           (if (file-exists? lib-path) (ffi-lib lib-path)))
         
         (define libgslcblas (find-lib safe-ffi-lib libgslcblas-paths ffi-lib?))
         (define libgsl (find-lib safe-ffi-lib libgsl-paths ffi-lib?))
         
         
         ; FIXME: Find correct path automatically

         ; from the "GNU Scientific Library for Lisp" project:
         ; 
         ;  (cffi:define-foreign-library libgslcblas
         ;      (:darwin
         ;       (:or "/opt/local/lib/libgslcblas.dylib" "/sw/lib/libgslcblas.dylib"
         ;      "/usr/local/lib/libgslcblas.dylib"))
         ;    (:cygwin (:or "/bin/cyggslcblas-0.dll"))
         ;    (:unix (:or "/usr/lib/libgslcblas.so.0" "/usr/lib/libgslcblas.so"
         ;          "/usr/lib64/libgslcblas.so.0" "/usr/lib64/libgslcblas.so"))
         ;    (t (:default "libgslcblas")))
         ;  
         ;  (cffi:define-foreign-library libgsl
         ;      (:darwin
         ;       (:or "/opt/local/lib/libgsl.dylib" "/sw/lib/libgsl.dylib"
         ;      "/usr/local/lib/libgsl.dylib"))
         ;    (:cygwin (:or "/bin/cyggsl-0.dll"))
         ;    (:unix (:or "/usr/lib/libgsl.so.0" "/usr/lib/libgsl.so"
         ;          "/usr/lib64/libgsl.so.0" "/usr/lib64/libgsl.so"))
         ;    (t (:default "libgsl")))
         
         
         ;; =============================================================
         ;;
         ;; Utilities
         ;;

         (define (to-immutable l)
           (if (mlist? l) (mlist->list l) l))
         
         (define (to-mutable l)
           (if (not (mlist? l)) (list->mlist l) l))
         
         (define (mlist->cblock l t)
           (list->cblock (to-immutable l) t))
         
         (define (cblock->mlist p t n)
           (to-mutable (cblock->list p t n)))
         
         (define (mlength l)
           (length (to-mutable l)))
         
         
         ;; =============================================================
         ;;
         ;; Miscellaneous bindings
         ;;

         (define _gsl-lnfact
           (get-ffi-obj 'gsl_sf_lnfact libgsl(_fun _int -> _double)))
         (define (gsl-lnfact n )
           (_gsl-lnfact (inexact->exact n)))

         (define _gsl-lngamma
           (get-ffi-obj 'gsl_sf_lngamma libgsl(_fun _double -> _double)))
         (define (gsl-lngamma n )
           (_gsl-lngamma (exact->inexact n)))

         ;;FIXME: convert this to mzscheme ffi...
         ;;(define _gsl-polygamma ((make-c-callout 'double '(unsigned-int double)) (dlsym libgsl "gsl_sf_psi_n"))) 
         ;;(define (gsl-polygamma n x) (_gsl-polygamma (inexact->exact n) (exact->inexact x)))



         ;; =============================================================
         ;;
         ;; RNG bindings
         ;;

         ;; this is the default RNG; it's fast and reliable
         (define gsl-rngt-taus (get-ffi-obj 'gsl_rng_taus libgsl _pointer))

         ;; According to
         ;; http://www.gnu.org/software/gsl/manual/html_node/Random-Number-Generator-Performance.html
         ;; gfsr4 and mt19937 are the other two fastest.
         ;; The best mathematically-proven quality are those based on the ranlux algorithm.
         ;; These are ordered by performance / quality:
         (define gsl-rngt-gfsr4 (get-ffi-obj 'gsl_rng_gfsr4 libgsl _pointer))
         (define gsl-rngt-mt19937 (get-ffi-obj 'gsl_rng_mt19937 libgsl _pointer))
         (define gsl-rngt-ranlxs0 (get-ffi-obj 'gsl_rng_ranlxs0 libgsl _pointer))
         (define gsl-rngt-ranlxs1 (get-ffi-obj 'gsl_rng_ranlxs1 libgsl _pointer))
         (define gsl-rngt-mrg (get-ffi-obj 'gsl_rng_mrg libgsl _pointer))
         (define gsl-rngt-ranlux (get-ffi-obj 'gsl_rng_ranlux libgsl _pointer))
         (define gsl-rngt-ranlxd1 (get-ffi-obj 'gsl_rng_ranlxd1 libgsl _pointer))
         (define gsl-rngt-ranlxs2 (get-ffi-obj 'gsl_rng_ranlxs2 libgsl _pointer))
         (define gsl-rngt-cmrg (get-ffi-obj 'gsl_rng_cmrg libgsl _pointer))
         (define gsl-rngt-ranlux389 (get-ffi-obj 'gsl_rng_ranlux389 libgsl _pointer))
         (define gsl-rngt-ranlxd2 (get-ffi-obj 'gsl_rng_ranlxd2 libgsl _pointer))
         (define gsl-rngt-ran3 (get-ffi-obj 'gsl_rng_ran3 libgsl _pointer))  ;; this is another fast one
         (define gsl-rngt-ran0 (get-ffi-obj 'gsl_rng_ran0 libgsl _pointer))
         (define gsl-rngt-ran1 (get-ffi-obj 'gsl_rng_ran1 libgsl _pointer))
         (define gsl-rngt-ran2 (get-ffi-obj 'gsl_rng_ran2 libgsl _pointer))

         ;; XXX there are more than these in gsl_rng.h ...

         ;; you need to allocate and destroy space for the state each RNG
         ;; maintains.
         (define gsl-rng-alloc
           (get-ffi-obj 'gsl_rng_alloc libgsl
             (_fun _pointer -> _pointer)))

         (define gsl-rng-free
           (get-ffi-obj 'gsl_rng_free libgsl
             (_fun _pointer -> _void)))

	 ;; seed the random number generator.  expects an integer.
         (define _gsl-rng-set
	   (get-ffi-obj 'gsl_rng_set libgsl (_fun _pointer _int -> _void)))
         (define (gsl-rng-set n . rng)
           (_gsl-rng-set (get-rng rng) (inexact->exact n)))

	 (define (gsl-rng-randomize)
	   (gsl-rng-set (time-nanosecond (current-time))))
         
         ;; allocates the default RNG, which is taus.
	 ;; also seeds the rng with current time in nanoseconds.
	 ;; slightly tricky, since we can't use get-rng yet!
         (define gsl-default-rng
	   (let ((return-value (gsl-rng-alloc gsl-rngt-taus)))
	     (_gsl-rng-set return-value (time-nanosecond (current-time)))
	     return-value))

         ;; we want to make the rng an optional argument.  factor out some logic.
         (define (get-rng rng) (if (equal? rng '()) gsl-default-rng (car rng)))


         ;; =============================================================
         ;;
         ;; Bindings for random numbers
         ;; 

	 ;; generate a random integer between 0 and n-1, inclusive
         (define _gsl-rng-uniform-int
	   (get-ffi-obj 'gsl_rng_uniform_int libgsl (_fun _pointer _int -> _int)))
         (define (gsl-rng-uniform-int n . rng)
           (_gsl-rng-uniform-int (get-rng rng) (inexact->exact n)))


	 ;; generate a random integer between 0 and n-1, inclusive
         (define _gsl-rng-uniform-pos
	   (get-ffi-obj 'gsl_rng_uniform_pos libgsl (_fun _pointer -> _double)))
         (define (gsl-rng-uniform-pos . rng)
           (_gsl-rng-uniform-pos (get-rng rng) ))


         ;; =============================================================
         ;;
         ;; Bindings for standard distributions
         ;; 

         ;; --------------------------------------------
         ;; (gsl-bernoulli p rng)

         (define _gsl-bernoulli
           (get-ffi-obj 'gsl_ran_bernoulli libgsl(_fun _pointer _double -> _int)))
         (define (gsl-bernoulli p . rng)
           (_gsl-bernoulli (get-rng rng) (exact->inexact p)))

         ;; (gsl-bernoulli-pdf value p)
         (define _gsl-bernoulli-pdf
           (get-ffi-obj 'gsl_ran_bernoulli_pdf libgsl(_fun _int _double -> _double)))
         (define (gsl-bernoulli-pdf value p)
           (_gsl-bernoulli-pdf (exact->inexact value) (exact->inexact p)))


         ;; --------------------------------------------
         ;; (gsl-beta a b rng)
         (define _gsl-beta
           (get-ffi-obj 'gsl_ran_beta libgsl (_fun _pointer _double _double -> _double)))
         (define (gsl-beta a b . rng)
           (_gsl-beta (get-rng rng) (exact->inexact a) (exact->inexact b)))

         ;; (gsl-beta-pdf value a b)
         (define _gsl-beta-pdf
           (get-ffi-obj 'gsl_ran_beta_pdf libgsl (_fun _double _double _double -> _double)))
         (define (gsl-beta-pdf value a b)
           (_gsl-beta-pdf (exact->inexact value) (exact->inexact a) (exact->inexact b)))


         ;; --------------------------------------------
         ;; (gsl-exponential mu rng)
         (define _gsl-exponential
           (get-ffi-obj 'gsl_ran_exponential libgsl(_fun _pointer _double -> _double)))
         (define (gsl-exponential mu . rng)
           (_gsl-exponential (get-rng rng) (exact->inexact mu)))

         ;; (gsl-exponential-pdf value mu)
         (define _gsl-exponential-pdf
           (get-ffi-obj 'gsl_ran_exponential_pdf libgsl (_fun _double _double -> _double)))
         (define (gsl-exponential-pdf value mu)
           (_gsl-exponential-pdf (exact->inexact value) (exact->inexact mu)))


         ;; --------------------------------------------
         ;; (gsl-gamma a b rng)
         (define _gsl-gamma
           (get-ffi-obj 'gsl_ran_gamma libgsl (_fun _pointer _double _double -> _double)))
         (define (gsl-gamma a b . rng)
           (_gsl-gamma (get-rng rng) (exact->inexact a) (exact->inexact b)))

         ;; (gsl-gamma-pdf value a b)
         (define _gsl-gamma-pdf
           (get-ffi-obj 'gsl_ran_gamma_pdf libgsl (_fun _double _double _double -> _double)))
         (define (gsl-gamma-pdf value a b)
           (_gsl-gamma-pdf (exact->inexact value) (exact->inexact a) (exact->inexact b)))


         ;; --------------------------------------------
         ;; (gsl-poisson mu rng)
         (define _gsl-poisson
           (get-ffi-obj 'gsl_ran_poisson libgsl (_fun _pointer _double -> _int)))
         (define (gsl-poisson mu . rng)
           (_gsl-poisson (get-rng rng) (exact->inexact mu)))

         ;; (gsl-poisson-pdf value mu)  note that value is cast to be an integer, of course.
         (define _gsl-poisson-pdf
           (get-ffi-obj 'gsl_ran_poisson_pdf libgsl (_fun _int _double -> _double)))
         (define (gsl-poisson-pdf value mu)
           (_gsl-poisson-pdf (inexact->exact value) (exact->inexact mu)))


         ;; --------------------------------------------
         ;; (gsl-gaussian sigma rng)
         (define (gsl-gaussian sigma . rng)
           ((get-ffi-obj 'gsl_ran_gaussian libgsl (_fun _pointer _double -> _double))
            (get-rng rng) (exact->inexact sigma)))

         ;; (gsl-gaussian-pdf value sigma)
         (define (gsl-gaussian-pdf value sigma)
           ((get-ffi-obj 'gsl_ran_gaussian_pdf libgsl (_fun _double _double -> _double))
            (exact->inexact value) (exact->inexact sigma)))

         ;; --------------------------------------------
         ;; (gsl-tdist nu rng)
         (define (gsl-tdist nu . rng)
           ((get-ffi-obj 'gsl_ran_tdist libgsl (_fun _pointer _double -> _double))
            (get-rng rng) (exact->inexact nu)))

         ;; (gsl-tdist-pdf value nu)
         (define (gsl-tdist-pdf value nu)
           ((get-ffi-obj 'gsl_ran_tdist_pdf libgsl (_fun _double _double -> _double))
            (exact->inexact value) (exact->inexact nu)))


         ;; --------------------------------------------
         ;; (gsl-multinomial pvals trials rng)
         ;; returns a list of the number of times each element in the list was sampled.

         (define _gsl-multinomial
           (get-ffi-obj 'gsl_ran_multinomial libgsl (_fun _pointer _int _int _pointer _pointer -> _void)))

         (define (gsl-multinomial pvals trials . rng)
           (begin
            (define result_ptr (vector->cblock (make-vector (mlength pvals) 0) _int) )
            (define pvals_ptr (mlist->cblock (map exact->inexact pvals) _double))
            (_gsl-multinomial (get-rng rng) (mlength pvals) (inexact->exact trials) pvals_ptr result_ptr)
            (cblock->mlist result_ptr _int (mlength pvals))
         ))

         ;; (gsl-multinomial-pdf cnts pvals)
         (define _gsl-multinomial-pdf
           (get-ffi-obj 'gsl_ran_multinomial_pdf libgsl (_fun _int _pointer _pointer -> _double)))

         (define (gsl-multinomial-pdf cnts pvals)
           (begin
            (define pvals_ptr (mlist->cblock (map exact->inexact pvals) _double))
            (define cnts_ptr (mlist->cblock (map inexact->exact cnts) _int) )
            (_gsl-multinomial-pdf (mlength pvals) pvals_ptr cnts_ptr)
         ))


         ;; (gsl-multinomial-lnpdf cnts pvals)
         (define _gsl-multinomial-lnpdf
           (get-ffi-obj 'gsl_ran_multinomial_lnpdf libgsl (_fun _int _pointer _pointer -> _double)))

         (define (gsl-multinomial-lnpdf cnts pvals)
           (begin
            (define pvals_ptr (mlist->cblock (map exact->inexact pvals) _double))
            (define cnts_ptr (mlist->cblock (map inexact->exact cnts) _int) )
            (_gsl-multinomial-lnpdf (mlength pvals) pvals_ptr cnts_ptr)
         ))

         ;; --------------------------------------------
         ;; (gsl-dirichlet alpha rng)

         (define _gsl-dirichlet
           (get-ffi-obj 'gsl_ran_dirichlet libgsl (_fun _pointer _int _pointer _pointer -> _void)))

         (define (gsl-dirichlet alpha . rng)
           (begin
            (define theta_ptr (vector->cblock (make-vector (mlength alpha) 0.0) _double) )
            (define alpha_ptr (mlist->cblock (map exact->inexact alpha) _double))
            (_gsl-dirichlet (get-rng rng) (mlength alpha) alpha_ptr theta_ptr)
            (cblock->mlist theta_ptr _double (mlength alpha))
         ))

         ;; (gsl-dirichlet-pdf theta alpha)
         (define _gsl-dirichlet-pdf
           (get-ffi-obj 'gsl_ran_dirichlet_pdf libgsl (_fun _int _pointer _pointer -> _double)))

         (define (gsl-dirichlet-pdf theta alpha)
           (begin
            (define theta_ptr (mlist->cblock (map exact->inexact theta) _double))
            (define alpha_ptr (mlist->cblock (map exact->inexact alpha) _double) )
            (_gsl-dirichlet-pdf (mlength alpha) alpha_ptr theta_ptr)
         ))

         ;; (gsl-dirichlet-lnpdf theta alpha)
         (define _gsl-dirichlet-lnpdf
           (get-ffi-obj 'gsl_ran_dirichlet_lnpdf libgsl (_fun _int _pointer _pointer -> _double)))

         (define (gsl-dirichlet-lnpdf theta alpha)
           (begin
            (define theta_ptr (mlist->cblock (map exact->inexact theta) _double))
            (define alpha_ptr (mlist->cblock (map exact->inexact alpha) _double) )
            (_gsl-dirichlet-lnpdf (mlength alpha) alpha_ptr theta_ptr)
         ))

         ;; --------------------------------------------

         ;; return the index of a 1 in a list of ones and zeros.
         ;; if this crashes with an error about #f, it could
         ;; be that gsl-multinomial returned an all-zero list...
         (define (find-one mylist)
           (- (mlength mylist) (mlength (member 1 mylist))))

         (define (gsl-discrete pvals . rng)
           (find-one (gsl-multinomial pvals 1 (get-rng rng))))

         (define (gsl-discrete-pdf pvals val)
           (list-ref pvals val))

         ;; =============================================================


)



