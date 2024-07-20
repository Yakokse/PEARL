(Start End Rules S_right) -> (Start End Rules S_right) with (Q Q1 Q2 S1 S2 S S_left RulesRev Rule Rules')

init:
	fi (Start = End)
		from stop
		else act1
	Rules' ^= Rules
	Q ^= Start
	'BLANK <- S
	exit

stop:
	entry
	Q ^= End
	S <- 'BLANK
	Rules' ^= Rules
	if (Start = End)
		goto init
		else act4

act1:
	fi ((Q = Q1) && (S = S1))
		from write
		else act2
	Rules' <- ((Q1 . (S1 . (S2 . Q2))) . Rules')
	if (!(RulesRev) && (Q = Start))
		goto init
		else act4

write:
	from act2
	S ^= S2
	S ^= S1
	Q ^= Q2
	Q ^= Q1
	goto act1

act2:
	fi ((Q = Q1) && (S1 = 'SLASH))
		from move
		else act3
	if ((Q = Q2) && (S = S2))
		goto write
		else act1

act3:
	fi Rules'
		from act4
		else reload
	((Q1 . (S1 . (S2 . Q2))) . RulesRev) <- RulesRev
	if ((Q = Q2) && (S1 = 'SLASH))
		goto move1
		else act2

reload:
	fi RulesRev
		from reload
		else act4
	(Rule . Rules') <- Rules'
	RulesRev <- (Rule . RulesRev)
	if Rules'
		goto reload
		else act3

act4:
	fi (!(RulesRev) && (Q = End))
		from stop
		else act1
	if !(RulesRev)
		goto reload
		else act3

move:
	fi (S2 = 'LEFT)
		from left
		else right
	Q ^= Q2
	Q ^= Q1
	goto act2

left:
	fi ((S_right = 'nil) && (S = 'BLANK))
		from left_1b
		else left_1p
	goto move

left_1b:
	from left1
	S ^= 'BLANK
	goto left

left_1p:
	from left1
	(S . S_right) <- S_right
	goto left

left1:
	fi (S_left = 'nil)
		from left_2b
		else left_2p
	if (S_right = 'nil)
		goto left_1b
		else left_1p

left_2b:
	from left2
	S ^= 'BLANK
	goto left1

left_2p:
	from left2
	S_left <- (S . S_left)
	goto left1

left2:
	from move1
	if ((S_left = 'nil) && (S = 'BLANK))
		goto left_2b
		else left_2p

right:
	fi ((S_left = 'nil) && (S = 'BLANK))
		from right_1b
		else right_1p
	goto move

right_1b:
	from right1
	S ^= 'BLANK
	goto right

right_1p:
	from right1
	(S . S_left) <- S_left
	goto right

right1:
	fi (S_right = 'nil)
		from right_2b
		else right_2p
	if (S_left = 'nil)
		goto right_1b
		else right_1p

right_2b:
	from right2
	S ^= 'BLANK
	goto right1

right_2p:
	from right2
	S_right <- (S . S_right)
	goto right1

right2:
	from move1
	if ((S_right = 'nil) && (S = 'BLANK))
		goto right_2b
		else right_2p

move1:
	from act3
	if (S2 = 'LEFT)
		goto left2
		else right2
