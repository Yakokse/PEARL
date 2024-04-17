(text) -> (chars counts) with (c n temp)

init: entry
      if text goto outer else stop

outer: fi chars from outerEnd else init
       (c . text) <- text
       n <- '1
       if text goto boolT else boolF

inner: from boolT
       n += '1
       (temp . text) <- text
       temp ^= c
       if text goto boolT else boolF

boolT: fi n = '1 from outer else inner
       if hd text = c goto inner else outerEnd

boolF: fi n = '1 from outer else inner
       goto outerEnd

outerEnd: fi text from boolT else boolF
          chars <- (c . chars)
          counts <- (n . counts)
          if text goto outer else stop

stop: fi chars from outerEnd else init
      exit
