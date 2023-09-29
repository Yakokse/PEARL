init: entry
      if dyn goto fst else snd

fst: from init
     elim ^= 'a
     goto stop

snd: from init
     elim ^= 'b
     goto stop

stop: fi dyn from fst else snd
      exit