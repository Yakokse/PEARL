init: entry
      if dyn = 0 goto fst else snd

fst: from init
     elim += 1
     goto stop

snd: from init
     elim += 2
     goto stop

stop: fi dyn = 0 from fst else snd
      exit