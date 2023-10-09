(key msg) -> (key msg) with (j t)

init:
	from loop
	t ^= '0
	j ^= '0
	exit

loop:
	fi (((msg / '2) * '2) = msg)
		from even
		else odd
	t -= (msg / '2)
	if (j = '0)
		goto init
		else loop1

even:
	from loop1
	t += msg
	msg += t
	goto loop

odd:
	from loop1
	msg -= key
	t += msg
	msg += (t + '1)
	goto loop

loop1:
	fi (j = '19)
		from stop
		else loop
	j -= '1
	if !((msg < key))
		goto odd
		else even

stop:
	entry
	j ^= '19
	t ^= '0
	goto loop1
