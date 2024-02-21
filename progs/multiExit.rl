(dyn elim) -> (elim dyn)
init: entry
      elim ^= '0
      if dyn = '5 goto fst else snd

fst: from init
     elim += '1
     goto stop

snd: from init
     elim += '2
     goto stop

stop: fi dyn = '5 from fst else snd
      dyn += elim
      exit