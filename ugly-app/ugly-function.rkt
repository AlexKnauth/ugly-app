#lang racket/base

(provide define-ugly-function
         define-ugly-macro
         (for-syntax ~seq))

(require syntax/parse/define
         "ugly-app.rkt"
         "placeholder.rkt"
         (for-syntax racket/base
                     racket/syntax
                     "ugly-name.rkt"
                     ))

(begin-for-syntax
  ;; [Syntax Id -> Any] Syntax [Listof Id] -> Any
  ;; (wrap-ellipses #'stx (list #'a #'b #'c)) = #'[[[stx a] b] c]
  (define (wrap-ellipses f stx ellipses)
    (for/fold ([stx stx]) ([ellipsis (in-list ellipses)])
      (f stx ellipsis)))

  ;; A UsagePatternOps is a:
  ;;   (usage-pattern-ops [Syntax [Listof Id] -> Syntax]
  ;;                      [Syntax [Listof Id] -> Syntax])
  (struct usage-pattern-ops [parameter-ellipses argument-ellipses])
  (define (list2-expr a b) #`(list #,a #,b))
  (define ops:function
    (usage-pattern-ops (λ (x els) x)
                       (λ (x els) (wrap-ellipses list2-expr x els))))
  (define ops:macro
    (usage-pattern-ops (λ (x els) (wrap-ellipses list x els))
                       (λ (x els) (wrap-ellipses list x els))))

  (define-syntax-class (usage-pattern-item1 name-parts ops)
    #:attributes [[part 1] [parameter 1] [argument 1] stxparse-pattern]
    [pattern x:id
      #:when (member #'x name-parts free-identifier=?)
      #:cut
      #:with stxparse-pattern #'{~literal x}
      #:with [part ...] #'[x]
      #:with [[parameter argument] ...] '()]
    [pattern {~and x:id {~not {~literal ...}}}
      #:when (not (member #'x name-parts free-identifier=?))
      #:with stxparse-pattern #'{~var x}
      #:with [part ...] '()
      #:with [[parameter argument] ...] #'[[x x]]]
    [pattern ({~and seq {~literal ~seq}} i ...)
      #:declare i (usage-pattern-item name-parts ops)
      #:with stxparse-pattern #'(seq i.stxparse-pattern ...)
      #:with [part ...] #'[i.part ... ...]
      #:with [parameter ...] #'[i.parameter ... ...]
      #:with [argument ...] #'[i.argument ... ...]])

  (define-splicing-syntax-class (usage-pattern-item name-parts ops)
    #:attributes [[part 1] [parameter 1] [argument 1] stxparse-pattern]
    [pattern {~seq {~var || (usage-pattern-item1 name-parts ops)}}]
    [pattern {~seq {~var i1 (usage-pattern-item1 name-parts ops)}
                   {~and ooo {~literal ...}}
                   ...+}
      #:with stxparse-pattern #'{~seq i1.stxparse-pattern ooo ...}
      #:with [part ...] #'[i1.part ...]
      #:with [parameter ...]
      (for/list ([x (in-list (attribute i1.parameter))])
        ((usage-pattern-ops-parameter-ellipses ops) x (attribute ooo)))
      #:with [argument ...]
      (for/list ([x (in-list (attribute i1.argument))])
        ((usage-pattern-ops-argument-ellipses ops) x (attribute ooo)))]
    )

  (define-syntax-class (usage-pattern name-parts ops)
    #:attributes [parameters class-id class-def]
    [pattern (pat ...)
      #:declare pat (usage-pattern-item name-parts ops)
      #:with parameters #'(pat.parameter ... ...)
      #:with parts-id (id-join #'(pat.part ... ...) #'-)
      #:with internal-id (generate-temporary #'parts-id)
      #:with class-id (generate-temporary #'parts-id)
      #:with class-def
      #'(define-syntax-class (class-id internal-id)
          #:attributes [output]
          [pattern (pat.stxparse-pattern ...)
            #:with output
            (with-syntax ([internal-id internal-id])
              #'(internal-id pat.argument ... ...))])])
  )



(define-syntax-parser define-ugly-function
  [(_ name-part:id ... [usage-pattern body:expr ...+] ...)
   #:declare usage-pattern (usage-pattern (attribute name-part) ops:function)
   #:with [internal-id ...] (generate-temporaries
                             (attribute usage-pattern.class-id))
   #:with parser (generate-temporary 'parser)
   #:with [[parser* _] ...] #'[[parser name-part] ...]
   #'(begin
       (define (internal-id . usage-pattern.parameters)
         body ...)
       ...
       (define-syntaxes [name-part ...]
         (let ()
           usage-pattern.class-def ...
           (define-syntax-class combined-class
             #:attributes [output]
             [pattern {~var || (usage-pattern.class-id
                                (quote-syntax internal-id))}]
             ...)
           (define parser
             (ugly-name-piece
              (...
               (syntax-parser
                 #:track-literals
                 [{~and :at-least-one-placeholder ~!
                        args:arguments/placeholders}
                  #:with {~var b combined-class} #'(args.argument ...)
                  #'(λ (args.parameter ...) b.output)]
                 [{~var || combined-class}
                  (attribute output)]))))
           (values parser* ...))))])

(define-syntax-parser define-ugly-macro
  [(_ name-part:id ... [usage-pattern body ...] ...)
   #:declare usage-pattern (usage-pattern (attribute name-part) ops:macro)
   #:with [internal-id ...] (generate-temporaries
                             (attribute usage-pattern.class-id))
   #:with parser (generate-temporary 'parser)
   #:with [[parser* _] ...] #'[[parser name-part] ...]
   #'(begin
       (define-simple-macro (internal-id . usage-pattern.parameters)
         body ...)
       ...
       (define-syntaxes [name-part ...]
         (let ()
           usage-pattern.class-def ...
           (define-syntax-class combined-class
             #:attributes [output]
             [pattern {~var || (usage-pattern.class-id
                                (quote-syntax internal-id))}]
             ...)
           (define parser
             (ugly-name-piece
              (syntax-parser
                #:track-literals
                [{~var || combined-class} (attribute output)])))
           (values parser* ...))))])

