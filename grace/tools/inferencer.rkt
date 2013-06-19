#lang racket
(require drracket/tool
         racket/class
         racket/gui/base
         racket/match
         framework
         racket/unit
         grace/lang/parse
         grace/lang/typecheck)

(provide start-inference-tool)

(define (parse-string in) (parse (object-name in) in))

(define inferencer%
  (class object%
    (super-new)
    (init-field definitions-text)    
    
    (define current-entry #f); (list start end var-name type-name type-def primitive? inf-type)
    (define end-of-first-line (send definitions-text line-end-position 0 #f))
    (define offset end-of-first-line)
    (define current-highlight-key #f)
    (define round-type 'prims)
    
    ;; Do the inference for primitives
    (define inference-list 
          (infer-prims (parse-string
                        (open-input-string 
                         (send definitions-text get-text 
                               ; exclude the #lang line
                               offset)))))
    
    (set! offset (- offset 1))
    
     ;; Define and initialize the frame
    (define frame (new frame% [label "Infer Types"]))
    (define msg (new message% [parent frame]
                     [label "Loading..."]
                     [auto-resize #t]))
    (define insert-button (new button% [parent frame]
                               [label "Insert"]
                               [callback (lambda (button event)
                                           (insert-type)
                                           (process-next))]))
    (define next-button (new button% [parent frame]
                             [label "Skip"]
                             [callback (lambda (button event)
                                         (process-next))]))
    (define insert-all-button (new button% [parent frame]
                             [label "Insert all"]
                             [callback (lambda (button event)
                                         (for ([_ inference-list])
                                           (insert-type)
                                           (process-next))
                                         (insert-type)
                                         (process-next))]))
    (define quit-button (new button% [parent frame]
                             [label "Quit"]
                             [callback (lambda (button event)
                                         (when current-highlight-key
                                           (send definitions-text unhighlight-ranges/key current-highlight-key))
                                         (send frame show #f))]))
    
    (define/public (insert-type)
      (match-define (list start end var-name type-name type-def primitive? inf-type) current-entry)
      (when (not primitive?)
        (displayln type-def)
        (send definitions-text insert type-def (+ start offset))
        (set! offset (+ offset (string-length type-def))))
      (define to-insert (format " : ~a" type-name))
      (send definitions-text insert to-insert (+ end offset))
      (set! offset (+ offset (string-length to-insert))))
    
    (define/public (process-next)
      (when current-highlight-key
        (send definitions-text unhighlight-ranges/key current-highlight-key))
      
      (if (empty? inference-list)
          (if (equal? round-type 'prims)
              ; start inference for object types
              (begin (set! round-type 'objects)
                     (set! offset end-of-first-line)
                     (set! current-highlight-key #f)
                     (set! inference-list (infer-objects (parse-string
                                                        (open-input-string 
                                                         (send definitions-text get-text 
                                                               ; exclude the #lang line
                                                               offset)))))
                     (set! offset (- offset 1))
                     (process-next))
              (send frame show #f)) ; we've processed everything, so hide the frame
          (begin
            (set! current-entry (car inference-list))
            (set! inference-list (cdr inference-list))
            (match-let ([(list start end var-name type-name type-def primitive? inf-type) current-entry])
              (set! current-highlight-key (format "inference:~a" start)) 
              (send definitions-text highlight-range (+ offset start) (+ offset end) "green" 
                    #:adjust-on-insert/delete? #t
                    #:key current-highlight-key)
              (send msg set-label (format "~a looks like a ~a" var-name type-name))))))
  
    (send frame show #t)
    (process-next)))


(define (start-inference-tool frame)
  (new inferencer% 
       [definitions-text (send frame get-definitions-text)]))

