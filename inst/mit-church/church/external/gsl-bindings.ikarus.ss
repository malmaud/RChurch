#!r6rs

;;
;; FFI bindings for various parts of GSL that church needs:
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
         
         (export gsl-lnfact gsl-lngamma gsl-polygamma
		 gsl-rng-set gsl-rng-randomize
		 gsl-rng-uniform-int gsl-rng-uniform-pos
                 gsl-bernoulli gsl-bernoulli-pdf gsl-beta gsl-beta-pdf
                 gsl-binomial gsl-binomial-pdf gsl-exponential gsl-exponential-pdf
                 gsl-gamma gsl-gamma-pdf gsl-poisson gsl-poisson-pdf gsl-gaussian gsl-gaussian-pdf
                 gsl-tdist gsl-tdist-pdf
                 gsl-multinomial gsl-multinomial-pdf gsl-multinomial-lnpdf
                 gsl-dirichlet gsl-dirichlet-pdf gsl-dirichlet-lnpdf
                 gsl-discrete gsl-discrete-pdf)
         
         (import (_srfi :1)
		 (only (ikarus) current-time time-nanosecond)
                 (ikarus foreign)
                 (only (ikarus) inexact->exact exact->inexact)
                 (church external gsl-paths)
                 (church external binding-utils)
                 ;(church utils rnrs)
                 (rnrs)
                 )
        
         
         ;; =============================================================
         ;;
         ;; Library paths
         ;;
      
         (define libgslcblas (find-lib (lambda (path) (dlopen path #t #f)) libgslcblas-paths pointer?))
         (define libgsl (find-lib (lambda (path) (dlopen path #t #f)) libgsl-paths pointer?))
  
        
         ;; =============================================================
         ;;
         ;; Miscellaneous bindings
         ;;
         
         (define _gsl-lnfact ((make-c-callout 'double '(unsigned-int)) (dlsym libgsl "gsl_sf_lnfact"))) 
         (define (gsl-lnfact n ) (_gsl-lnfact (inexact->exact n)))

         (define _gsl-lngamma ((make-c-callout 'double '(double)) (dlsym libgsl "gsl_sf_lngamma"))) 
         (define (gsl-lngamma n ) (_gsl-lngamma (exact->inexact n)))

         (define _gsl-polygamma ((make-c-callout 'double '(unsigned-int double)) (dlsym libgsl "gsl_sf_psi_n"))) 
         (define (gsl-polygamma n x) (_gsl-polygamma (inexact->exact n) (exact->inexact x)))
         
         ;; =============================================================
         ;;
         ;; RNG bindings
         ;;
         
         ;; this is the default RNG; it's fast and reliable
         (define gsl-rngt-taus (ptr-deref (dlsym libgsl "gsl_rng_taus")))
         
         ;; According to
         ;; http://www.gnu.org/software/gsl/manual/html_node/Random-Number-Generator-Performance.html
         ;; gfsr4 and mt19937 are the other two fastest.
         ;; The best mathematically-proven quality are those based on the ranlux algorithm.
         ;; These are ordered by performance / quality:
         (define gsl-rngt-gfsr4 (ptr-deref (dlsym libgsl "gsl_rng_gfsr4")))
         (define gsl-rngt-mt19937 (ptr-deref (dlsym libgsl "gsl_rng_mt19937")))
         (define gsl-rngt-ranlxs0 (ptr-deref (dlsym libgsl "gsl_rng_ranlxs0")))
         (define gsl-rngt-ranlxs1 (ptr-deref (dlsym libgsl "gsl_rng_ranlxs1")))
         (define gsl-rngt-mrg (ptr-deref (dlsym libgsl "gsl_rng_mrg")))
         (define gsl-rngt-ranlux (ptr-deref (dlsym libgsl "gsl_rng_ranlux")))
         (define gsl-rngt-ranlxd1 (ptr-deref (dlsym libgsl "gsl_rng_ranlxd1")))
         (define gsl-rngt-ranlxs2 (ptr-deref (dlsym libgsl "gsl_rng_ranlxs2")))
         (define gsl-rngt-cmrg (ptr-deref (dlsym libgsl "gsl_rng_cmrg")))
         (define gsl-rngt-ranlux389 (ptr-deref (dlsym libgsl "gsl_rng_ranlux389")))
         (define gsl-rngt-ranlxd2 (ptr-deref (dlsym libgsl "gsl_rng_ranlxd2")))
         (define gsl-rngt-ran3 (ptr-deref (dlsym libgsl "gsl_rng_ran3")))  ;; this is another fast one
         (define gsl-rngt-ran0 (ptr-deref (dlsym libgsl "gsl_rng_ran0")))
         (define gsl-rngt-ran1 (ptr-deref (dlsym libgsl "gsl_rng_ran1")))
         (define gsl-rngt-ran2 (ptr-deref (dlsym libgsl "gsl_rng_ran2")))
         
         ;; XXX there are more than these in gsl_rng.h ...
         
         ;; you need to allocate and destroy space for the state each RNG
         ;; maintains.
         (define gsl-rng-alloc
           ((make-c-callout  'pointer '(pointer)) (dlsym libgsl "gsl_rng_alloc")))
         
         (define gsl-rng-free
           ((make-c-callout 'void '(pointer)) (dlsym libgsl "gsl_rng_free")))
         
	 ;; seed the random number generator.  expects an integer.
         (define _gsl-rng-set
           ((make-c-callout 'void '(pointer unsigned-int)) (dlsym libgsl "gsl_rng_set")))
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
           ((make-c-callout 'unsigned-int '(pointer unsigned-int)) (dlsym libgsl "gsl_rng_uniform_int")))
         (define (gsl-rng-uniform-int n . rng)
           (_gsl-rng-uniform-int (get-rng rng) (inexact->exact n)))


	 ;; generate a random real between 0 and n-1, inclusive
         (define _gsl-rng-uniform-pos
           ((make-c-callout 'double '(pointer)) (dlsym libgsl "gsl_rng_uniform_pos")))
         (define (gsl-rng-uniform-pos . rng)
           (_gsl-rng-uniform-pos (get-rng rng) ))

         
         
         ;; =============================================================
         ;;
         ;; Bindings for standard distributions
         ;; 
         
         ;; --------------------------------------------
         ;; (gsl-bernoulli p rng)
         
         (define _gsl-bernoulli
           ((make-c-callout 'unsigned-int '(pointer double)) (dlsym libgsl "gsl_ran_bernoulli")))
         
         (define (gsl-bernoulli p . rng)
           (_gsl-bernoulli (get-rng rng) (exact->inexact p)))
         
         ;; (gsl-bernoulli-pdf value p)
         (define _gsl-bernoulli-pdf
           ((make-c-callout 'double '(unsigned-int double)) (dlsym libgsl "gsl_ran_bernoulli_pdf")))
         
         (define (gsl-bernoulli-pdf value p)
           (_gsl-bernoulli-pdf (inexact->exact value) (exact->inexact p)))


         ;; --------------------------------------------
         ;; (gsl-binomial p k rng)

         (define _gsl-binomial
           ((make-c-callout 'unsigned-int '(pointer double unsigned-int)) (dlsym libgsl "gsl_ran_binomial")))
         
         (define (gsl-binomial p n . rng)
           (_gsl-binomial (get-rng rng) (exact->inexact p) (inexact->exact n)))

         ;; (gsl-binomial-pdf value p n)
         (define _gsl-binomial-pdf
           ((make-c-callout 'double '(unsigned-int double unsigned-int)) (dlsym libgsl "gsl_ran_binomial_pdf")))

         (define disp
           (lambda args
             (for-each display args)))
         
         (define (gsl-binomial-pdf value p n)
           (_gsl-binomial-pdf (inexact->exact value) (exact->inexact p) (inexact->exact n)))
         
         
         ;; --------------------------------------------
         ;; (gsl-beta a b rng)
         
         (define _gsl-beta
           ((make-c-callout 'double '(pointer double double)) (dlsym libgsl "gsl_ran_beta")))
         
         (define (gsl-beta a b . rng)
           (_gsl-beta (get-rng rng) (exact->inexact a) (exact->inexact b)))
         
         ;; (gsl-beta-pdf value a b)
         (define _gsl-beta-pdf
           ((make-c-callout 'double '(double double double)) (dlsym libgsl "gsl_ran_beta_pdf")))
         
         (define (gsl-beta-pdf value a b)
           (_gsl-beta-pdf (exact->inexact value) (exact->inexact a) (exact->inexact b)))
         
         
         ;; --------------------------------------------
         ;; (gsl-exponential mu rng)
         
         (define _gsl-exponential
           ((make-c-callout 'double '(pointer double)) (dlsym libgsl "gsl_ran_exponential")))
         
         (define (gsl-exponential mu . rng)
           (_gsl-exponential (get-rng rng) (exact->inexact mu)))
         
         ;; (gsl-exponential-pdf value mu)
         (define _gsl-exponential-pdf
           ((make-c-callout 'double '(double double)) (dlsym libgsl "gsl_ran_exponential_pdf")))
         
         (define (gsl-exponential-pdf value mu)
           (_gsl-exponential-pdf (exact->inexact value) (exact->inexact mu)))
         
         
         ;; --------------------------------------------
         ;; (gsl-gamma a b rng)
         
         (define _gsl-gamma
           ((make-c-callout 'double '(pointer double double)) (dlsym libgsl "gsl_ran_gamma")))
         
         (define (gsl-gamma a b . rng)
           (_gsl-gamma (get-rng rng) (exact->inexact a) (exact->inexact b)))
         
         ;; (gsl-gamma-pdf value a b)
         (define _gsl-gamma-pdf
           ((make-c-callout 'double '(double double double)) (dlsym libgsl "gsl_ran_gamma_pdf")))
         
         (define (gsl-gamma-pdf value a b)
           (_gsl-gamma-pdf (exact->inexact value) (exact->inexact a) (exact->inexact b)))
         
         
         ;; --------------------------------------------
         ;; (gsl-poisson mu rng)
         
         (define _gsl-poisson
           ((make-c-callout 'unsigned-int '(pointer double)) (dlsym libgsl "gsl_ran_poisson")))
         
         (define (gsl-poisson mu . rng)
           (_gsl-poisson (get-rng rng) (exact->inexact mu)))
         
         ;; (gsl-poisson-pdf value mu)  note that value is cast to be an integer, of course.
         (define _gsl-poisson-pdf
           ((make-c-callout 'double '(unsigned-int double)) (dlsym libgsl "gsl_ran_poisson_pdf")))
         
         (define (gsl-poisson-pdf value mu)
           (_gsl-poisson-pdf (inexact->exact value) (exact->inexact mu)))
         
         
         ;; --------------------------------------------
         ;; (gsl-gaussian sigma rng)
         
         (define _gsl-gaussian
           ((make-c-callout 'double '(pointer double)) (dlsym libgsl "gsl_ran_gaussian")))
         
         (define (gsl-gaussian sigma . rng)
           (_gsl-gaussian (get-rng rng) (exact->inexact sigma)))
         
         ;; (gsl-gaussian-pdf value sigma)
         (define _gsl-gaussian-pdf
           ((make-c-callout 'double '(double double)) (dlsym libgsl "gsl_ran_gaussian_pdf")))
         
         (define (gsl-gaussian-pdf value sigma)
           (_gsl-gaussian-pdf (exact->inexact value) (exact->inexact sigma)))
         
         ;; --------------------------------------------
         ;; (gsl-tdist nu rng)
         ;; the student t-distribution (standard, not generalized)
         
         (define _gsl-tdist
           ((make-c-callout 'double '(pointer double)) (dlsym libgsl "gsl_ran_tdist")))
         
         (define (gsl-tdist nu . rng)
           (_gsl-tdist (get-rng rng) (exact->inexact nu)))
         
         ;; (gsl-tdist-pdf value nu)
         (define _gsl-tdist-pdf
           ((make-c-callout 'double '(double double)) (dlsym libgsl "gsl_ran_tdist_pdf")))
         
         (define (gsl-tdist-pdf value nu)
           (_gsl-tdist-pdf (exact->inexact value) (exact->inexact nu)))
         
         ;; --------------------------------------------
         ;; (gsl-multinomial pvals trials rng)
         ;; returns a list of the number of times each element in the list was sampled.
         
         (define _gsl-multinomial
           ((make-c-callout 'void '(pointer unsigned-int unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_multinomial")))
         
         (define (gsl-multinomial pvals trials . rng)
           (begin
             (define result_ptr (vector->cblock (make-vector (length pvals) 0) 'unsigned-int) )
             (define pvals_ptr (list->cblock (map exact->inexact pvals) 'double))
             (_gsl-multinomial (get-rng rng) (length pvals) (inexact->exact trials) pvals_ptr result_ptr)
             (cblock->list result_ptr 'unsigned-int (length pvals))
             ;; XXX memory leak -- we should free result_ptr and pvals_ptr
             ))
         
         ;; (gsl-multinomial-pdf cnts pvals)
         (define _gsl-multinomial-pdf
           ((make-c-callout 'double '(unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_multinomial_pdf")))
         
         (define (gsl-multinomial-pdf cnts pvals)
           (begin
             (define pvals_ptr (list->cblock (map exact->inexact pvals) 'double))
             (define cnts_ptr (list->cblock (map inexact->exact cnts) 'unsigned-int) )
             (_gsl-multinomial-pdf (length pvals) pvals_ptr cnts_ptr)
             ;; XXX memory leak -- we should free result_ptr and cnts_ptr
             ))
         
         
         ;; (gsl-multinomial-lnpdf cnts pvals)
         (define _gsl-multinomial-lnpdf
           ((make-c-callout 'double '(unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_multinomial_lnpdf")))
         
         (define (gsl-multinomial-lnpdf cnts pvals)
           (begin
             (define pvals_ptr (list->cblock (map exact->inexact pvals) 'double))
             (define cnts_ptr (list->cblock (map inexact->exact cnts) 'unsigned-int) )
             (_gsl-multinomial-lnpdf (length pvals) pvals_ptr cnts_ptr)
             ;; XXX memory leak -- we should free result_ptr and cnts_ptr
             ))
         
         ;; --------------------------------------------
         ;; (gsl-dirichlet alpha rng)
         
         (define _gsl-dirichlet
           ((make-c-callout 'void '(pointer unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_dirichlet")))
         
         (define (gsl-dirichlet alpha . rng)
           (begin
             (define theta_ptr (vector->cblock (make-vector (length alpha) 0.0) 'double) )
             (define alpha_ptr (list->cblock (map exact->inexact alpha) 'double))
             (_gsl-dirichlet (get-rng rng) (length alpha) alpha_ptr theta_ptr)
             (cblock->list theta_ptr 'double (length alpha))
             ;; XXX memory leak -- we should free result_ptr and cnts_ptr
             ))
         
         ;; (gsl-dirichlet-pdf theta alpha)
         (define _gsl-dirichlet-pdf
           ((make-c-callout 'double '(unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_dirichlet_pdf")))
         
         (define (gsl-dirichlet-pdf theta alpha)
           (begin
             (define theta_ptr (list->cblock (map exact->inexact theta) 'double))
             (define alpha_ptr (list->cblock (map exact->inexact alpha) 'double) )
             (_gsl-dirichlet-pdf (length alpha) alpha_ptr theta_ptr)
             ;; XXX memory leak -- we should free result_ptr and cnts_ptr
             ))
         
         ;; (gsl-dirichlet-lnpdf theta alpha)
         (define _gsl-dirichlet-lnpdf
           ((make-c-callout 'double '(unsigned-int pointer pointer)) (dlsym libgsl "gsl_ran_dirichlet_lnpdf")))
         
         (define (gsl-dirichlet-lnpdf theta alpha)
           (begin
             (define theta_ptr (list->cblock (map exact->inexact theta) 'double))
             (define alpha_ptr (list->cblock (map exact->inexact alpha) 'double) )
             (_gsl-dirichlet-lnpdf (length alpha) alpha_ptr theta_ptr)
             ;; XXX memory leak -- we should free result_ptr and cnts_ptr
             ))
         
         ;; --------------------------------------------
         
         ;; return the index of a 1 in a list of ones and zeros.
         ;; if this crashes with an error about #f, it could
         ;; be that gsl-multinomial returned an all-zero list...
         (define (find-one mylist)
           (- (length mylist) (length (member 1 mylist))))
         
         (define (gsl-discrete pvals . rng)
           (find-one (gsl-multinomial pvals 1 (get-rng rng))))
         
         (define (gsl-discrete-pdf pvals val)
           (list-ref pvals val))
         
         ;; =============================================================
         
)
