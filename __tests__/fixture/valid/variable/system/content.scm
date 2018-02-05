(
  (frame
    (slot-set! system foo "test0")
    (set! system bar "test1")
    (text (clear #t) #"~|(slot-ref system foo)| ~|(ref system bar)|")
  )
)
