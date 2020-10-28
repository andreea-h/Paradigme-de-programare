#lang racket
(struct position (x y))
(define my_pos (position 1 2))
(position-x my_pos)

(define-struct new_pos (a b c))

(define my_new_pos (new_pos 1 2 3))
my_new_pos
(new_pos-c my_new_pos)

(struct struct-bird (x y y_speed) #:transparent)
(define my_bird (struct-bird 1 2 3))
(struct-bird-x my_bird)
(struct-copy struct-bird my_bird [x 4])
