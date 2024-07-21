
(import (chicken pretty-print))
(import (chicken format))
(import srfi-1)

(define input 3005290)

(define table-helper
 (lambda (n i)
    (cond
    ((= n 1) (format #t "~a  <done> ~%" n))
    (#t  (let* ((index (floor (/ n 2)))
                (index2 (+ index i))
                (index3 (modulo index2 n))
                (index4 (modulo (+ index2 index) n)))            
            (format #t "  ~a   ~a   ~a  ~a  ~a   ~a ~%" n index index2 index3 index4 i)
            (table-helper (- n 1) (+ i 1)))))))


(define table
(lambda (n)
 (table-helper n 0)))



    
        
    
          
