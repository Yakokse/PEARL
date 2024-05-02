(chars) -> (text) with (c temp)
// specialized w.r.t count

init_1:
	entry
	(c . chars) <- chars
	if text
		goto boolT_1_4
		else outer_1_4

boolT_1_4:
	from init_1
	assert(!((hd(text) = c)))
	goto outer_1_4

outer_1_4:
	fi text
		from boolT_1_4
		else init_1
	text <- (c . text)
	(c . chars) <- chars
	if text
		goto boolT_1_8
		else inner_1_9

boolT_1_8:
	from outer_1_4
	assert(!((hd(text) = c)))
	goto inner_1_9

inner_1_9:
	fi text
		from boolT_1_8
		else outer_1_4
	temp ^= c
	text <- (temp . text)
	assert((hd(text) = c))
	temp ^= c
	text <- (temp . text)
	assert((hd(text) = c))
	text <- (c . text)
	(c . chars) <- chars
	if text
		goto boolT_1_16
		else inner_1_17

boolT_1_16:
	from inner_1_9
	assert(!((hd(text) = c)))
	goto inner_1_17

inner_1_17:
	fi text
		from boolT_1_16
		else inner_1_9
	temp ^= c
	text <- (temp . text)
	assert((hd(text) = c))
	text <- (c . text)
	(c . chars) <- chars
	if text
		goto boolT_1_22
		else outer_1_22

boolT_1_22:
	from inner_1_17
	assert(!((hd(text) = c)))
	goto outer_1_22

outer_1_22:
	fi text
		from boolT_1_22
		else inner_1_17
	text <- (c . text)
	exit
