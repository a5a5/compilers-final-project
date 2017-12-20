#lang racket

; Run this file or use the following at the REPL
(require "cps.rkt") ;using ref
(require "utils.rkt") ;check
(require "desugar.rkt") ;using ref
(require "closure-convert.rkt") ;using my own. not gonna 0-cfa for now, as that will fuck up my function argument number validating. 
(require "top-level.rkt") ;using ref



(define ((path->name [dir "."]) p)
  (define filename (last (string-split (path->string p) "/")))
  (string-append dir "/" (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/"))))

(define (tests-list [dir "."])
  (map
   (lambda (path)
     (string->path
      (string-append "final_tests/" dir "/"
                     (path->string path))))
   (filter (lambda (path)
             (define p (path->string path))
             (member (last (string-split p ".")) '("scm")))
           (directory-list (string-append "final_tests/" dir "/")))))

(define public-tests (tests-list))
(define student-tests (tests-list "student_tests"))

(define (run-test name)
  (define scm (read-begin (open-input-file name #:mode 'text)))
  (display name)
  ;(test-top-level top-level scm)
  (define p (closure-convert (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level scm)))))))))
  ;(eval-llvm (proc->llvm p))
  (test-proc->llvm proc->llvm p)
  )

(define (run-errors name)
  (define scm (read-begin (open-input-file name #:mode 'text)))
  (define p (closure-convert (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level scm)))))))))
  (define clo-value (eval-proc p))
  (define llvm-value (eval-llvm (proc->llvm p)))
  (equal? clo-value llvm-value)
  )

(define (run-public-tests)
  (map run-test public-tests)
  )

(run-public-tests)
(define test "final_tests/student_tests/many-args.scm")
(run-errors test)
