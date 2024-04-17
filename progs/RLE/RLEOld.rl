(text) -> (chars counts) with (c n temp)

init: entry
      if text goto outer else stop

outer: fi chars from outerEnd else init
       c ^= hd text
       n <- '0
       goto inner

inner: fi n = '0 from outer else boolT
       n += '1
       (temp . text) <- text
       temp ^= c
       if text goto boolT else outerEnd
//  "text && hd text = c", but do not evaluate RHS if text=nil

boolT: from inner
       if hd text = c goto inner else outerEnd

outerEnd: fi text from boolT else inner
          chars <- (c . chars)
          counts <- (n . counts)
          if text goto outer else stop

stop: fi chars from outerEnd else init
      exit
