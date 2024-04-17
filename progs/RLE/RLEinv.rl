(chars counts text) -> (text) with (c n temp)

init:
	fi text
		from outer
		else stop
	exit

outer:
	fi text
		from boolT
		else boolF
	'1 <- n
	text <- (c . text)
	if chars
		goto outerEnd
		else init

inner:
	fi text
		from boolT
		else boolF
	temp ^= c
	text <- (temp . text)
	n -= '1
	goto boolT

boolT:
	fi (hd(text) = c)
		from inner
		else outerEnd
	if (n = '1)
		goto outer
		else inner

boolF:
	from outerEnd
	if (n = '1)
		goto outer
		else inner

outerEnd:
	fi text
		from outer
		else stop
	(n . counts) <- counts
	(c . chars) <- chars
	if text
		goto boolT
		else boolF

stop:
	entry
	if chars
		goto outerEnd
		else init
