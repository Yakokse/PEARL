(x1) -> (x2 y1 y2) with (e)

init_1:
	entry
	if x1
		goto init_1_1_expl_1
		else init_1_2_expl_1

init_1_1_expl_1:
	from init_1
	assert(!(x2))
	(e . x1) <- x1
	x2 <- (e . x2)
	if x1
		goto loop_1_5
		else stop_1_5

loop_1_5:
	from init_1_1_expl_1
	assert(x2)
	(e . x1) <- x1
	x2 <- (e . x2)
	if x1
		goto loop_1_8
		else stop_1_8

loop_1_8:
	from loop_1_5
	assert(x2)
	(e . x1) <- x1
	x2 <- (e . x2)
	assert(!(x1))
	assert(x2)
	y2 <- '(3.(2.(1.nil)))
	goto stop_x1_2_12

stop_x1_2_12:
	fi ((y1 = 'nil) && (y2 = '(3.(2.(1.nil)))))
		from loop_1_8
		else stop_1_8
	goto stop_x1_4_12

stop_x1_4_12:
	fi (((y1 = 'nil) && (y2 = '(3.(2.(1.nil))))) || ((y1 = '(3.nil)) && (y2 = '(2.(1.nil)))))
		from stop_x1_2_12
		else stop_x3_4_12
	exit

stop_1_8:
	from loop_1_5
	assert(x2)
	y1 <- '(3.nil)
	y2 <- '(2.(1.nil))
	goto stop_x1_2_12

stop_1_5:
	from init_1_1_expl_1
	assert(x2)
	y1 <- '(2.(3.nil))
	y2 <- '(1.nil)
	goto stop_x3_4_12

stop_x3_4_12:
	fi ((y1 = '(2.(3.nil))) && (y2 = '(1.nil)))
		from stop_1_5
		else init_1_2_expl_1
	goto stop_x1_4_12

init_1_2_expl_1:
	from init_1
	assert(!(x2))
	y1 <- '(1.(2.(3.nil)))
	goto stop_x3_4_12
