(S_right) -> (S_right) with (S S_left)

init_1:
	entry
	S <- 'BLANK
	assert((S = 'BLANK))
	S ^= 'BLANK
	S ^= 'BLANK
	goto act2_1_6

act2_1_6:
	fi (S = 'BLANK)
		from init_1
		else write_1_67
	assert(!((S = 'SLASH)))
	if ((S_left = 'nil) && (S = 'BLANK))
		goto right_1b_1_10
		else right_1p_1_10

right_1b_1_10:
	from act2_1_6
	S ^= 'BLANK
	goto right1_1_10

right1_1_10:
	fi (S_left = 'nil)
		from right_1b_1_10
		else right_1p_1_10
	if (S_right = 'nil)
		goto right_2b_1_10
		else right_2p_1_10

right_2b_1_10:
	from right1_1_10
	S ^= 'BLANK
	goto right2_1_10

right2_1_10:
	fi ((S_right = 'nil) && (S = 'BLANK))
		from right_2b_1_10
		else right_2p_1_10
	if (S = '0)
		goto write_1_12
		else act2_1_12

write_1_12:
	from right2_1_10
	S ^= '0
	S ^= '1
	goto act2_1_14

act2_1_14:
	fi (S = '1)
		from write_1_12
		else write_1_24
	goto act2_1_18

act2_1_18:
	fi (S = 'BLANK)
		from act2_1_67
		else act2_1_14
	assert(!((S = 'SLASH)))
	if ((S_right = 'nil) && (S = 'BLANK))
		goto left_1b_1_22
		else left_1p_1_22

left_1b_1_22:
	from act2_1_18
	S ^= 'BLANK
	goto left1_1_22

left1_1_22:
	fi (S_right = 'nil)
		from left_1b_1_22
		else left_1p_1_22
	if (S_left = 'nil)
		goto left_2b_1_22
		else left_2p_1_22

left_2b_1_22:
	from left1_1_22
	S ^= 'BLANK
	goto left2_1_22

left2_1_22:
	fi ((S_left = 'nil) && (S = 'BLANK))
		from left_2b_1_22
		else left_2p_1_22
	if (S = '0)
		goto write_1_24
		else act2_1_24

write_1_24:
	from left2_1_22
	S ^= '0
	S ^= '0
	assert((S = '0))
	goto act2_1_14

act2_1_24:
	from left2_1_22
	assert((S = 'BLANK))
	S ^= 'BLANK
	S ^= 'BLANK
	assert((S = 'BLANK))
	'BLANK <- S
	exit

left_2p_1_22:
	from left1_1_22
	(S . S_left) <- S_left
	goto left2_1_22

left_1p_1_22:
	from act2_1_18
	S_right <- (S . S_right)
	goto left1_1_22

act2_1_12:
	from right2_1_10
	if (S = '1)
		goto write_1_67
		else act2_1_67

write_1_67:
	from act2_1_12
	S ^= '1
	S ^= '0
	assert((S = '0))
	goto act2_1_6

act2_1_67:
	from act2_1_12
	assert((S = 'BLANK))
	S ^= 'BLANK
	S ^= 'BLANK
	goto act2_1_18

right_2p_1_10:
	from right1_1_10
	(S . S_right) <- S_right
	goto right2_1_10

right_1p_1_10:
	from act2_1_6
	S_left <- (S . S_left)
	goto right1_1_10
