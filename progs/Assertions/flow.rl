(x) -> (x y)

init: entry
      if x goto A else B

A: from init
   if x goto C else D

B: from init
   y <- '1
   if x goto C else D

C: fi x from A else B
   y <- '2
   goto stop

D: fi x from A else B
   goto stop

stop: fi x from C else D
      exit
