(l1) -> (l1) with (tmpE tmpL)

init_1:
	entry
	if l1
		goto rev1_1_3
		else join_1_3

rev1_1_3:
	fi tmpL
		from rev1_1_3
		else init_1
	(tmpE . l1) <- l1
	tmpL <- (tmpE . tmpL)
	if l1
		goto rev1_1_3
		else join_1_3

join_1_3:
	fi tmpL
		from rev1_1_3
		else init_1
	l1 <- tmpL
	exit
