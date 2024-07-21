
(import (chicken pretty-print))
(import (chicken format))
(import srfi-1)

(define input 3005290)

(set! input 5)

(define-record node value next)

(define make-circ
  (lambda (n)
    (let ((hd #f)
          (tl #f))
      (letrec ((foo (lambda (i)
                      (let ((tmp (make-node i #f)))
                        (cond
                         ((not hd) 
                          (set! hd tmp)
                          (set! tl tmp))
                         (#t
                          (node-next-set! tl tmp)
                          (set! tl tmp)))
                         
                        (cond
                         ((= i n) (node-next-set! tl hd))
                         (#t 
                          (foo (+ i 1))))))))
        (foo n)
        hd))))


(define iter
  (lambda (node)
    (let ((hd node)
          (flag #f))
      (letrec ((foo (lambda (node)
                      (cond
                       ((and flag (eq? node hd)) 'done)
                       (#t (format #t "~a " (node-value node))
                           (set! flag #t)
                           (foo (node-next node)))))))
        (foo hd)))))
    
    
        
    
          
