(x y z) -> (x y z)

init: entry
      assert(y)
      if z goto fst else snd

fst: from init
     assert(x)
     assert(y)
     goto stop

snd: from init
     assert(x)
     assert(y)
     goto stop

stop: fi z from fst else snd
      assert(x)
      exit
