(lines) -> (lines) with (v1 v2 v3)

init_1:
	entry
	v1 <- lines['0]
	v2 <- lines['1]
	v3 <- lines['3]
	v3 ^= (v1 && v2)
	lines['3] <- v3
	lines['1] <- v2
	lines['0] <- v1
	v1 <- lines['0]
	v2 <- lines['1]
	v2 ^= v1
	lines['1] <- v2
	lines['0] <- v1
	v1 <- lines['1]
	v2 <- lines['2]
	v3 <- lines['3]
	v3 ^= (v1 && v2)
	lines['3] <- v3
	lines['2] <- v2
	lines['1] <- v1
	v1 <- lines['1]
	v2 <- lines['2]
	v2 ^= v1
	lines['2] <- v2
	lines['1] <- v1
	v1 <- lines['0]
	v2 <- lines['1]
	v2 ^= v1
	lines['1] <- v2
	lines['0] <- v1
	exit
