(key msg) -> (key msg) with (j t)

init: entry
      j ^= '0
      t ^= '0
      goto loop

loop: fi j = '0 from init else loop1
      t += msg / '2
      if (msg / '2) * '2 = msg goto even else odd

even: from loop
      msg -= t
      t -= msg
      goto loop1

odd: from loop
     msg -= t + '1
     t -= msg
     msg += key
     goto loop1

loop1: fi !(msg < key) from odd else even
       j += '1
       if j = '19 goto stop else loop

stop: from loop1
      t ^= '0
      j ^= '19
      exit
