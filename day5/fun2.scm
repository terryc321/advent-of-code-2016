
(import scheme)
(import (chicken format))
(import (chicken sort))

(import srfi-1)

(import md5)
(import md5 message-digest-byte-vector)

#|
(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))
|#

(define input "ojvtpuvg")

(define (five? s)
  (string=? (substring s 0 5) "00000"))



(message-digest-string (md5-primitive) "abc")
(message-digest-string (md5-primitive) "")


(define (test)
  (letrec ((loop (lambda (i)
		   (let ((digest (message-digest-string (md5-primitive) (format #f "~a~a" "abc" i))))
		     (when (five? digest)
		       (format #t "~a : ~a ~%" digest i))
		     (loop (+ i 1))))))
    (loop 0)))

#|
#;190> (test)
00000155f8105dff7f56ee10fa9b9abd : 3231929  >>> 1st
000008f82c5b3924a1ecbebf60344e00 : 5017308 
00000f9a2c309875e05c5a5d09f1b8c4 : 5278568 
000004e597bd77c5cd2133e9d885fe7e : 5357525 
0000073848c9ff7a27ca2e942ac10a4c : 5708769 
00000a9c311683dbbf122e9611a1c2d4 : 6082117 
000003c75169d14fdb31ec1593915cff : 8036669 
0000000ea49fd3fc1b2f10e02d98ee96 : 8605828   <<<< 8th
000006e42e097c536b8be5179d65f327 : 8609554 
000007b9278b049b172742aa82b5119a : 8760605 
000009d6e11733ceb6566b9c925a0770 : 9495334 
00000cf8353c7d266a990865ea529f26 : 10767910 
00000d64ba3bbc8102ec6179e495d88e : 11039607 

18f47a30 

|#


(define (search)
  (letrec ((loop (lambda (i)
		   (let ((digest (message-digest-string (md5-primitive) (format #f "~a~a" input i))))
		     (when (five? digest)
		       (format #t "~a : ~a ~%" digest i))
		     (loop (+ i 1))))))
    (loop 0)))



#|

1050cbbd  solution ?


1 0 5 0 c b b d
_ _ _ _ _ _ _ _ 
0 1 2 3 4 5 6 7

000004c52f7523dcea0ae987bb4bb7ae : 1469591     4 : c
000005b6777c6a6a5a72d3593ee1bade : 1925351     5 : b
0000049c67c129f031d6d2712e3e011d : 4310992      xx
00000307d284ec5fe32c12546f61d675 : 4851204     3 : 0
00000c4b121a0b7dceb8f719e3e5b9d1 : 6610226     xx c ?
00000101e84b5e967cba0ba19c7e7e00 : 6840976     1 : 0
00000574e86e49ffc208b84e771f3487 : 9504234     xx 5:7
00000439e8d28b6d251a65563b7c09d1 : 10320588    xx 4 : 3

taking first letter after five zeros on md5 gives password as 
4543c154


some other values computed .....

000008b0db39adef6a1d517303fea769 : 11009275   xx 8 : b
00000cf7dcf231d21fe7fdde75ddc435 : 11540947   xx c
00000bfdb76ea16ec5a2ebcab4e3ae0c : 12705714   xx b
0000097dd5e65fd1437a5df439a4788a : 13057078   xx 9 : 7
0000025a843b2da11af739013e61606f : 13615245   2 : 5
000004c59a9346e28b5874a9931665dd : 14718986   xx
000007d5f6040c1a402d7b5994b1e061 : 16706030   7 : d
00000c85b6afcb3487bcf9dd25a8d397 : 17045303   xx
0000015e5a89c4d43c138ec7216377f7 : 20797756   xx
000005433f1e5cb8c968edd5809e23e8 : 21799376   xx
00000daa272097f561153938c9156eb4 : 22569314 
00000e828dc841618678aeed7a4af074 : 22717846 
00000f0434b9dd57d34a30dd213e2227 : 23056264 
000006b0b288f96dcfc18f6a2414968b : 23173947   6 : b
00000c434af05fa7a5ebe670b07c7c34 : 23680041 
0000001c18c78b4ca1896ebc9692c67d : 27649592   0 : 1
00000ddf8f916b8b708b00e9ccf6fae0 : 29723327 
000008332ea7d376a1dcd7d8ecb52b02 : 30227478 
00000af71a5519d0148e2bb2cb4f00a5 : 30652370 
0000092c9445c74befe78ffd7cd2a06a : 31383475 
0000048e7d50ab7518caee1940dcec95 : 31603351 
00000697b0f82482641399504a265bc7 : 32374562 
000002f0a079ab7c4a3c8cc564dd1ead : 33108188 
000009333a9f9cf33b5a9369a4a5e1bf : 33700684 

|#
			
