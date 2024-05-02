(chars counts text) -> (text) with (c n temp)

stop: entry
	  if chars goto outerEnd else init

outerEnd: fi text from outer else stop
	      (n . counts) <- counts
	      (c . chars) <- chars
	      if text goto boolT else boolF

boolT: fi hd text = c from inner else outerEnd
	   if n = '1 goto outer else inner

boolF: from outerEnd
	   if n = '1 goto outer else inner

inner: fi text from boolT else boolF
	   temp ^= c
	   text <- (temp . text)
	   n -= '1
	   goto boolT

outer: fi text from boolT else boolF
	   '1 <- n
	   text <- (c . text)
	   if chars goto outerEnd else init

init: fi text from outer else stop
	  exit
