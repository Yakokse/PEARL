proc In
// Input:
// L list
// A item
// Output:
// (A.(L.IsIn))
init: entry (A.L)
      (A.(L.Count)) <- call Count (A.L)
      if ('0 < Count) goto isIn else isNotIn
isIn: from init
      IsIn ^='True
      goto done
isNotIn: from init
         IsIn ^= 'False
         goto done
done: fi IsIn = 'True from isIn else isNotIn
      (A.L) <- uncall Count (A.(L.Count))
      exit (A.(L.IsIn))

proc Count
// Input:
// L list
// A item
// Output:
// (A.(L.Count))
init: entry (A.L)
      Count ^= '0
      goto loop
loop: fi L' from loop2 else init
      if L goto loop1 else done
loop1: from loop
        (head.L) <- L
        if head = A goto match else loop2
loop2: fi head = A from match else loop1
       L' <- (head.L')
       goto loop
match: from loop1
       Count += '1
       goto loop2
done: from loop
      L <- call Reverse L'
      exit (A.(L.Count))

proc Reverse
    init: entry L
          goto loop
    loop: fi L' from loop1 else init
        if L goto loop1 else done
    loop1: from loop
        (head.L) <- L
        L' <- (head.L')
        goto loop
    done: from loop
          exit L'

