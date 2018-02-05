(
  (frame
    (slot-set! current foo "test0")
    (set! current bar "test1")
    (text (clear #t) #"~|(slot-ref current foo)| ~|(ref current bar)|")
  )
)
