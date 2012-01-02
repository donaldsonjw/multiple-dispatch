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

(module multiple-dispatch
   (export
    (class md-method
       (specializer-list (default '()))
       (entry (default (lambda ( . args) args))))
    (add-md-generic-method! generic proc specializer-list
        #!optional (gendef? #f))
    (collect-applicable-methods generic spec-list)
    (get-md-generic-methods-cache generic)
    (add-md-generic-methods-cache! generic cache)))


(define +md-generic-methods-cache+ (create-hashtable))

(define (add-md-generic-methods-cache! generic cache)
   (hashtable-put! +md-generic-methods-cache+ generic cache))

(define (get-md-generic-methods-cache generic)
   (if (hashtable-contains? +md-generic-methods-cache+ generic)
       (hashtable-get +md-generic-methods-cache+ generic)
       (error "get-md-generic-methods-cache" "no method cache" generic)))


(define (class-precedence-list class)
   (let loop ((p (class-super class))
	      (precendence-list '()))
      (if p
	  (loop (class-super p)
		(cons p precendence-list))
	  (reverse precendence-list))))

(define (subclass? class1 class2)
   (or (eq? class1 class2)
       (find (lambda (c) (eq? c class2)) (class-precedence-list class1))))


(define (get-md-generic-methods generic)
   (let ((p (procedure-ref generic 1)))
      (if (pair? p)
	  (cdr p)
	  '())))

(define (add-md-generic-method! generic proc specializer-list
	   #!optional (gendef? #f))
   (let* ((methods (get-md-generic-methods generic))
	  (exist? (let loop ((mthds methods))
		     (if (pair? mthds)
			 (let ((curr::md-method (car mthds)))
			    (if (every? eq? specializer-list
				   (-> curr specializer-list))
			     #t
			     (loop (cdr mthds))))))))
      ;(print "gendef?: " gendef?)
      ;(print "methods: " methods)

      ;;; check to make sure the method is compatible with the generic function
      (when (not gendef?)
	  (let ((g-spec-list (car (procedure-ref generic 1))))
	     (when (not (every? subclass? specializer-list g-spec-list))
		(error "add-md-generic-method!"
		   "method incompatible with generic function"
		   specializer-list))))
      
      (if exist?
	  (error "add-md-generic-method!" "method already defined for "
	     specializer-list)
	  (if (and gendef? (null? methods))
	      (procedure-set! generic 1
		 (cons specializer-list
		    (cons (instantiate::md-method
			     (specializer-list specializer-list)
			     (entry proc))
		       methods)))
	      (let ((g-spec-list (car (procedure-ref generic 1))))
		 (procedure-set! generic 1
		    (cons g-spec-list
		       (cons (instantiate::md-method
				(specializer-list specializer-list)
				(entry proc))
			  methods))))))))
	    
	 
(define (applicable-method? method::md-method classes-of-dispatch-args)
   (let ((specializer-list (-> method specializer-list)))
      (every? subclass? classes-of-dispatch-args specializer-list))) 


(define (collect-applicable-methods generic spec-list)
   (let ((classes-of-dispatch-args spec-list))
      (let loop ((methods (get-md-generic-methods generic))
		 (applicable-methods '()))
	 #;(print "methods: " methods)
	 (if (pair? methods)
	     (if (applicable-method? (car methods) classes-of-dispatch-args)
		 (loop (cdr methods)
		       (cons (car methods) applicable-methods))
		 (loop (cdr methods)
		       applicable-methods))
	     (topological-sort applicable-methods)))))


(define (more-specific-method? method1::md-method method2::md-method)
   (every? subclass?
	   (-> method1 specializer-list )
	   (-> method2 specializer-list)))

(define (topological-sort applicable-methods)
   (sort more-specific-method? applicable-methods))





	
