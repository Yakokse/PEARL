(chars counts text) -> (text) with (c n temp)

init:
	fi text from outer else stop
	exit

outer:
	from inner
	'0 <- n
	c ^= hd text
	if chars goto outerEnd else init

inner:
	fi text from boolT else outerEnd
	temp ^= c
	text <- (temp . text)
	n -= '1
	if n = '0 goto outer else boolT

boolT:
	fi hd text = c from inner else outerEnd
	goto inner

outerEnd:
    fi text from outer else stop
	(n . counts) <- counts
	(c . chars) <- chars
	if text goto boolT else inner

stop: entry
      if chars goto outerEnd else init
