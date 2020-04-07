#lang racket

(let [(do (and (display "a ")
               (delay (and (display
                            "done") 2))))
      (whatto (λ (what) (and (display "w
                    "))))]
                             (and (display "d ")
                                  (whatto do)))



