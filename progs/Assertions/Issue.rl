(dyn) -> (stat1 dyn stat2)

init_1:
	entry
	if dyn
		goto p1_1_1
		else p2_1_1

p1_1_1:
	from init_1
	assert(dyn)
	if dyn
		goto p4_1_2
		else p5_1_2

p4_1_2:
	from p1_1_1
	stat1 <- '1
	goto p4_1_1_e1_2_expl_6

p4_1_1_e1_2_expl_6:
	fi (stat1 = '1)
		from p4_1_2
		else p4_1_3
	assert(dyn)
	stat1 += dyn
	stat2 ^= '1
	goto stop_x1_2_6

stop_x1_2_6:
	fi (stat2 = '1)
		from p4_1_1_e1_2_expl_6
		else p5_1_1_e1_2_expl_6
	exit

p5_1_2:
	from p1_1_1
	stat1 <- '1
	goto p5_1_1_e1_2_expl_6

p5_1_1_e1_2_expl_6:
	fi (stat1 = '1)
		from p5_1_2
		else p5_1_3
	assert(!(dyn))
	stat1 += dyn
	stat2 ^= '2
	goto stop_x1_2_6

p2_1_1:
	from init_1
	assert(!(dyn))
	if dyn
		goto p4_1_3
		else p5_1_3

p4_1_3:
	from p2_1_1
	stat1 <- '2
	goto p4_1_1_e1_2_expl_6

p5_1_3:
	from p2_1_1
	stat1 <- '2
	goto p5_1_1_e1_2_expl_6
