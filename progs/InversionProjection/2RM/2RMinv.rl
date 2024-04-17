(Prog Y) -> (Prog X) with (i Prog' ProgRev Sign)

init:
	fi Prog'
		from loop
		else stop
	Prog' ^= Prog
	'0 <- Y
	exit

loop:
	fi (Sign = 'Inv)
		from revOp
		else regOp
	Prog' <- ((Sign . i) . Prog')
	if (!(ProgRev) && (Y = '0))
		goto init
		else loopEnd

loopJoin:
	fi Prog'
		from loopEnd
		else loopReload
	((Sign . i) . ProgRev) <- ProgRev
	if (Sign = 'Inv)
		goto revOpEnd
		else regOpEnd

loopReload:
	fi ProgRev
		from loopReload
		else loopEnd
	((Sign . i) . Prog') <- Prog'
	ProgRev <- ((Sign . i) . ProgRev)
	if Prog'
		goto loopReload
		else loopJoin

loopEnd:
	fi (!(ProgRev) && (X = '0))
		from stop
		else loop
	if ProgRev
		goto loopJoin
		else loopReload

stop:
	entry
	X <- '0
	Prog' ^= Prog
	if Prog'
		goto loopEnd
		else init

regOp:
	fi (i = 'Swap)
		from regSwap
		else regOp1
	goto loop

regSwap:
	from regOpEnd
	(Y . X) <- (X . Y)
	goto regOp

regOp1:
	fi (i = 'Sum)
		from regComb
		else regAdd
	goto regOp

regComb:
	from regOp2
	X -= Y
	goto regOp1

regAdd:
	from regOp2
	X -= i
	goto regOp1

regOp2:
	from regOpEnd
	if (i = 'Sum)
		goto regComb
		else regAdd

regOpEnd:
	from loopJoin
	if (i = 'Swap)
		goto regSwap
		else regOp2

revOp:
	fi (i = 'Swap)
		from revSwap
		else revOp1
	goto loop

revSwap:
	from revOpEnd
	(X . Y) <- (Y . X)
	goto revOp

revOp1:
	fi (i = 'Sum)
		from revComb
		else revAdd
	goto revOp

revComb:
	from revOp2
	X += Y
	goto revOp1

revAdd:
	from revOp2
	X += i
	goto revOp1

revOp2:
	from revOpEnd
	if (i = 'Sum)
		goto revComb
		else revAdd

revOpEnd:
	from loopJoin
	if (i = 'Swap)
		goto revSwap
		else revOp2
