(dyn) -> (elim1 elim2 dyn)
init: entry
      if dyn goto fst else snd

fst: from init
     elim1 ^= 'a
     elim2 ^= 'a
     goto stop

snd: from init
     elim1 ^= 'a
     elim2 ^= 'b
     goto stop

stop: fi dyn from fst else snd
      exit