(seed n raise) -> (seed nums) with (bit seedrev seedb tmp)

// Random numbers using a LFSR, gives n+1 numbers based on seed
// https://www.youtube.com/watch?v=Ks1pw1X22y4

init: entry
      nums ^= raise
      if n = '0 goto stop else loop

loop: fi nums from loop1 else init
      tmp ^= seed
      nums <- (tmp . nums)
      (bit . seed) <- seed
      if bit = '0 goto xor0 else xor1

xor0: from loop
      bit += seed # '0 
      goto xor

xor1: from loop
      bit -= seed # '0 
      goto xor

xor: fi bit = seed # '0 from xor0 else xor1
     goto append

append: fi seedrev from append else xor
        (seedb . seed) <- seed
        seedrev <- (seedb . seedrev)
        if seed goto append else append1

append1: from append
         seedrev <- (bit . seedrev)
         goto append2

append2: fi seed from append2 else append1
         (seedb . seedrev) <- seedrev
         seed <- (seedb . seed)
         if seedrev goto append2 else loop1

loop1: from append2
       n -= '1
       if n = '0 goto stop else loop

stop: fi nums from loop1 else init
      n ^= '0
      exit