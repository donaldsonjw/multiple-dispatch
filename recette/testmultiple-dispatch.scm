;;;; Copyright(c) 2011, 2012 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of multiple-dispatch.
;;;;
;;;;     multiple-dispatch is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     multiple-dispatch is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with multiple-dispatch.  If not, see
;;;;     <http://www.gnu.org/licenses/>.

(module testmultiple-dispatch
   (library multiple-dispatch)
   (main main))

;;;; testing infrastructure copied from the recette for Bigloo's pthread library

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
	     (print "error.")
	     (print "   ==> provided: [" provided
		    "]\n       expected: ["
		    (if (procedure? res) (res 'result) res)
		    "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))


;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (for-each (lambda (pvn)
		   (apply test pvn))
		(if (null? tests)
		    (reverse *tests*)
		    (reverse (filter (lambda (t) (memq (car t) tests))
				     *tests*))))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))


;;;; cond-expand	   
(define-test cond-expand 
   (cond-expand	   
      (multiple-dispatch #t) 
      (else #f))	   
   :result #t)



(define-class animal)
(define-class dog::animal)
(define-class cat::animal)
(define-class pig::animal)
(define-class not-an-animal)


(define-md-generic (doit a1::animal a2::animal)
   "two animals")


(define-md-method (doit a1::dog a2::dog)
   "two dogs")

(define-md-method (doit a1::cat a2::cat)
   "two cats")

(define-md-method (doit a1::dog a2::cat)
   "a dog and a cat")


(define-test two-animals
   (doit (instantiate::animal)
      (instantiate::animal))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "two animals"
		  (string=? "two animals"
		     v))))

(define-test two-dogs
   (doit (instantiate::dog)
      (instantiate::dog))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "two dogs"
		  (string=? "two dogs"
		     v))))

(define-test two-cats
   (doit (instantiate::cat)
      (instantiate::cat))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "two cats"
		  (string=? "two cats"
		     v))))


(define-test a-dog-and-a-cat
   (doit (instantiate::dog)
      (instantiate::cat))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a dog and a cat"
		  (string=? "a dog and a cat"
		     v))))

(define-test a-cat-and-a-dog
   (doit (instantiate::cat)
      (instantiate::dog))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "two animals"
		  (string=? "two animals"
		     v))))

(define-test call-with-not-an-animals
   (with-handler (lambda (e)
		    #t)
		 (begin
		    (define-md-method (doit a1::not-an-animal
					     a2::not-an-animal)
		       (print "two not-an-animals"))
		    #f))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "an exception"
		  v)))


(define-test call-with-atomic-types-error
   (with-handler (lambda (e)
		    #t)
		 (begin
		    (doit 5 6)
		    #f))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "an exception"
		  v)))

(define-md-method (doit a1::cat a2::pig)
   (string-append "a cat and a pig are also "
      (call-next-method)))


(define-test call-next-method
   (doit (instantiate::cat)
      (instantiate::pig))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a cat and a pig are also two animals"
		  (string=? v "a cat and a pig are also two animals"))))