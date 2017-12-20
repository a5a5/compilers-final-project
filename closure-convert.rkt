#lang racket

(require "utils.rkt")
(require "desugar.rkt")
(require "cps.rkt")

(provide closure-convert
         proc->llvm
         simplify-ae
         remove-varargs)



; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
                  (foldr (lambda (ae xs+wrap)
                           (define gx (gensym 'arg))
                           (if (symbol? ae)
                               (cons (cons ae (car xs+wrap))
                                     (cdr xs+wrap))
                               (cons (cons gx (car xs+wrap))
                                     (lambda (e)
                                       (match ae
                                              [`(lambda ,xs ,body) 
                                               `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                                  ,((cdr xs+wrap) e))]
                                              [`',dat
                                               `(let ([,gx ',dat])
                                                  ,((cdr xs+wrap) e))])))))
                         (cons '() wrap)
                         aes))
    (wrap+ xs))
  (match e
         [`(let ([,x (lambda ,xs ,elam)]) ,e0)
          `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(simplify-ae e0))]

         [`(let ([,x (prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]
         [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]

         [`(if (lambda . ,_) ,et ,ef)
          (simplify-ae et)]
         [`(if '#f ,et ,ef)
          (simplify-ae ef)]
         [`(if ',dat ,et ,ef)
          (simplify-ae et)]
         [`(if ,(? symbol? x) ,et ,ef)
          `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

         [`(apply (lambda ,xs ,ebody) ,ae)
          (define xf (gensym 'lam))
          (simplify-ae
           `(let ([,xf (lambda ,xs ,ebody)])
              (apply ,xf ,ae)))]
         [`(apply ,(? symbol? x) (lambda ,xs ,ebody))
          (define xf (gensym 'lam))
          (simplify-ae
           `(let ([,xf (lambda ,xs ,ebody)])
              (apply ,x ,xf)))]
         [`(apply ',dat ,ae)
          (pretty-print '(warning: applying datum))
          `',dat]
         [`(apply ,(? symbol? x) ',dat)
          (define xd (gensym 'datum))
          `(let ([,xd ',dat])
             (apply ,x ,xd))]
         [`(apply ,(? symbol? x0) ,(? symbol? x1))
          `(apply ,x0 ,x1)]
         
         [`(,aes ...)
          (wrap-aes aes (lambda (xs) xs))]))


; Helper to remove vararg lambdas/callsites
(define (remove-varargs e) 
  (match e
         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(remove-varargs e0))]

         [`(let ([,x (prim ,op ,xs ...)]) ,e0)
          `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
         [`(let ([,x (apply-prim ,op ,y)]) ,e0)
          `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
    
;put in a check that make sure that na is an empty list and that rvp is actually a cons cell (!!!!!!!!!!)
    [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
          ; turns (xs ...) into x and immediately into (x)
          ; by adding the needed car/cdr calls and let bindings
          (define gx (gensym 'rvp))
          (define dumb (gensym 'dumb))
          (define overmsg (gensym 'msg))
          (define erro (gensym 'err))
          (define na (gensym 'na))
          (define gx+e
            (foldr (lambda (x gx+e)
                     (define gx (gensym 'rvp))
                     (define stupid (gensym 'stupid))
                     (define errormsg (gensym 'msg))
                     (define err (gensym 'err))
                     (cons gx
                           
                           `(let ([,stupid (prim cons? ,gx)])
                                (if ,stupid
                                    (let ([,x (prim car ,gx)])
                                      (let ([,(car gx+e) (prim cdr ,gx)])
                                        ,(cdr gx+e)))
                                    (let ([,errormsg '"Error:_Not_enough_arguments"]) (let ([,err (prim halt ,errormsg)]) (,err ,stupid)))
                                    ))
                           ))
                   (cons na (remove-varargs `(let ([,dumb (prim null? ,na)])
                                               (if ,dumb
                                                   ,body
                                                   (let ([,overmsg '"Error:_too_many_arguments"]) (let ([,erro (prim halt ,overmsg)]) (,erro ,dumb)))
                                                   )
                                            )))
                   xs))
          `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
             ,(remove-varargs e0))]
    
         [`(let ([,x (lambda ,y ,body)]) ,e0)
          `(let ([,x (lambda (,y) ,(remove-varargs body))])
             ,(remove-varargs e0))]
    
         [`(if ,x ,e0 ,e1)
          `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
    
         [`(apply ,f ,args)
          (define co (gensym 'co))
          (define msg (gensym 'msg))
          (define err (gensym 'err))
          `(let ([,co (prim procedure? ,f)])
             (if ,co
              (,f ,args)
              (let ([,msg '"Error:_Non-function_application"]) (let ([,err (prim halt ,msg)]) (,err ,msg)))))]
    
         [`(,f ,xs ...)
          (define l (gensym 'l))
          (define (build-list xs prev-name)
            (define new-name (gensym 'cons))
            (if (null? xs)
                `(,f ,prev-name)
                `(let ([,new-name (prim cons ,(car xs) ,prev-name)]) ,(build-list (cdr xs) new-name))
                )
            )
          (define co (gensym 'co))
          (define msg (gensym 'msg))
          (define err (gensym 'err))
          `(let ([,co (prim procedure? ,f)])
             (if ,co
                 (let ([,l '()]) ,(build-list (reverse xs) l))
                 (let ([,msg '"Error:_Non-function_application"]) (let ([,err (prim halt ,msg)]) (,err ,msg)))))
          ;`(let ([,l (prim list ,@xs)]) (,f ,l)) ;lmao prim list has been removed, so we now need to turn l into a list using cons or smth
          ]))


; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))

  (define (bottom-up exp procs)
    (match exp
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
                     (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                     (cons (+ 1 cnt)
                                           `(let ([,x (env-ref ,envx ,cnt)])
                                              ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))]
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
                     (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]
      
      [`(let ([,x (apply-prim ,op ,v)]) ,e)
       (match-define `(,e+ ,free+ ,procs+) (bottom-up e procs))
       `((let ([,x (apply-prim ,op ,v)]) ,e+)
         ,(set-add (set-remove free+ x) v)
         ,procs+)
       ]

      [`(let ([,x ,(? symbol? v)]) ,e0)
          (match-define `(,e+ ,free+ ,procs+) (bottom-up e0 procs))
          `((let ([,x ,v]) ,e+)
            ,(set-add (set-remove free+ x) v)
            ,procs+)
          ]

      ))
  
  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  `((proc (main) ,main-body) . ,procs))






; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define strs (mutable-set))
(define (proc->llvm procs)
  (define (global vars)
    (if (null? vars)
        ""
        (string-append "@" (c-name (car vars)) " = private unnamed_addr constant ["
                       (number->string (+ 1 (string-length (symbol->string (car vars))))) " x i8] c\"" (symbol->string (car vars))
                       "\\00\", align 8\n"
                       (global (cdr vars)))
        )
                   
    )
  
  (string-append (handle-main (car procs))
                 "\ndefine i32 @main() {\n\tcall fastcc void @proc_main()\n\tret i32 0\n}\n"
                 (if (null? (cdr procs))
                     ""
                     (handle-procs (cdr procs)))
                 (global  (set->list strs)))
  )
(define (params xs)
  (if (null? xs)
      ""
      (if (null? (cdr xs))
          (string-append "i64 %"
                         (c-name (car xs)))
       
          (string-append "i64 %"
                         (c-name (car xs))
                         ", "
                         (params (cdr xs))
                         )
          )
      )
  )
(define (walk exp)
  (match exp
    [`(let ([,x ',(? null? dat)]) ,e)
     (string-append "%" (c-name x) " = call i64 @const_init_null()\n"
                    (walk e)
                    )
     ]
    [`(let ([,x ',(? integer? dat)]) ,e)
     (string-append "%" (c-name x) " = call i64 @const_init_int(i64 " (number->string dat) ")\n"
                    (walk e)
                    )
     ]
    [`(let ([,x ',(? char? dat)]) ,e)
     (string-append "%" (c-name x) " = call i64 @const_init_char(i64 " (string dat) ")\n"
                    (walk e)
                    )
     ]
    
    [`(let ([,x ',(? boolean? dat)]) ,e) ;single bit integer either 1 or 0. probably anyway
     (define bool
       (if dat
           "true()"
           "false()"))
     (string-append "%" (c-name x) " = call i64 @const_init_" bool "\n"
                    (walk e)
                    )
     ]


    [`(let ([,x ',(? string? date)]) ,e)

     ;%2 = alloca i8*, align 8
     ;store i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.44, i32 0, i32 0), i8** %2, align 8
     ;
     ;%5 = load i8*, i8** %2, align 8
     ;%6 = call i64 @const_init_string(i8* %5)
     ;%2 is str, %5 is strptr, %6 is x

     (define dat (string->symbol date))
     (set-add! strs dat)
     (define str (symbol->string (gensym '%str)))
     (define strptr (symbol->string (gensym '%strptr)))
     
     (define dat-length (number->string (+ 1 (string-length (symbol->string dat)))))
     
     (string-append "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = call i64 @const_init_string(i8* getelementptr inbounds([" dat-length " x i8], [" dat-length " x i8]* @" (c-name dat) ", i32 0, i32 0))\n"
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]
    
    [`(let ([,x ',(? symbol? dat)]) ,e)

     (set-add! strs dat)
     (define str (symbol->string (gensym '%str)))
     (define strptr (symbol->string (gensym '%strptr)))
     (define dat-length (number->string (+ 1 (string-length (symbol->string dat)))))
     (string-append "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = call i64 @const_init_symbol(i8* getelementptr inbounds([" dat-length " x i8], [" dat-length " x i8]* @" (c-name dat) ", i32 0, i32 0))\n"
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]

    [`(let ([,x (apply-prim ,op ,v)]) ,e) ;v has to be a list. u will pass in a pointer to the list (%v). that means above case must be done too
     (string-append "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = call i64 @" (prim-applyname op) "(i64 %" (c-name v) ")\n"
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]
      
    [`(let ([,x (prim ,op ,xs ...)]) ,e)
     (string-append "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = call i64 @" (prim-name op) "(" (params xs) ")\n"
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]
    
    [`(let ([,x (make-closure ,lamb ,envs ...)]) ,e) ;should only have one env param anyway
     (define closureptr (symbol->string (gensym '%cloptr)))
     (define (set-env envs count)
       (define eptr (symbol->string (gensym '%evar)))
       (if (null? envs)
           ""
           (string-append eptr " = getelementptr inbounds i64, i64* " closureptr  ", i64 " (number->string count)
                          "\nstore i64 %" (c-name (car envs)) ", i64* " eptr ", align 8\n"
                          (set-env (cdr envs) (+ 1 count))
                          )
           )
       )

     (define functionptr (symbol->string (gensym '%fptr)))
     (define function (symbol->string (gensym '%f)))
    
     (string-append 
                    closureptr " = call i64* @alloc(i64 " (number->string (+ 8 (* 8 (length envs)))) ")\n"
                    (set-env envs 1)
                    functionptr " = getelementptr inbounds i64, i64* " closureptr ", i64 0\n"
                    function " = ptrtoint void(i64,i64)* @" (c-name lamb) " to i64\n" ;make new pointer to lambda
                    "store i64 " function ", i64* " functionptr ", align 8\n"  ;set functionptr to function
                    "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = ptrtoint i64* " closureptr " to i64\n"
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]
    
    [`(let ([,x (env-ref ,env ,index)]) ,e)
     (define cloptr (symbol->string (gensym '%eclo)))
     (define envptr (symbol->string (gensym '%envptr)))
     (string-append cloptr " = inttoptr i64 %" (c-name env) " to i64*\n" envptr " = getelementptr inbounds i64, i64* " cloptr ", i64 " (number->string index) "\n"
                    "%" (c-name x) "ptr = alloca i64, align 8\n"
                    "%" (c-name x) " = load i64, i64* " envptr ", align 8\n" ;not super sure why align 8. 8 bytes is 64 bits, all our things are stored as i64s. pointers are usually only 32 bits tho. probably idk
                    "store volatile i64 %" (c-name x) ", i64* %" (c-name x) "ptr, align 8\n"
                    (walk e)
                    )
     ]
    
    [`(clo-app ,f ,args ...) 
     (define closureptr (symbol->string (gensym '%cloptr)))
     (define functionaddr (symbol->string (gensym '%i0ptr)))
     (define function (symbol->string (gensym '%f)))
     (define functionptr (symbol->string (gensym '%fptr)))

     (string-append closureptr " =  inttoptr i64 %" (c-name f) " to i64*\n"
                    functionaddr " = getelementptr inbounds i64, i64* " closureptr  ", i64 0\n"
                    function " = load i64, i64* " functionaddr ", align 8\n"
                    functionptr " = inttoptr i64 " function " to void (i64, i64)*\n"
                    "musttail call fastcc void " functionptr "(i64 %" (c-name f) ", " (params args) ")\n"
                    )
     ]
    [`(if ,cond ,t-e ,f-e)
     (define true-label (symbol->string (gensym 'true)))
     (define false-label (symbol->string (gensym 'false)))
     (define new-cond (symbol->string (gensym '%bool)))
     (string-append new-cond " = icmp ne i64 %" (c-name cond) ", 15\n"
                    "br i1 " new-cond ", label %" true-label ", label %" false-label
                    "\n\n"
                    true-label ":\n"
                    (walk t-e)
                    "ret void\n\n"
                    false-label ":\n"
                    (walk f-e)
                    )
     ]
    )
  )

(define (handle-main main)
  (match-define `(proc (main) ,main-body) main)
  
  (string-append "\ndefine void @proc_main() {\n"
                 (walk main-body)
                 "ret void\n}\n"
                 )
  )


(define (handle-procs procs)
  (match-define `((proc (,proc-name ,env-vars ...) ,body) . ,rest) procs)
  (string-append "\ndefine void @"
                 (c-name proc-name)
                 "("
                 (params env-vars)
                 ") {\n"
                 (walk body)
                 "ret void\n}\n"
                 (if (null? rest)
                     ""
                     (handle-procs rest))
                 )
  )
