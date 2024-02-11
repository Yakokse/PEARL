(stat dyn) -> (stat dyn)

init: entry
      if dyn goto dynpath else statpath

statpath: from init
          if dyn goto left else right

left: from statpath
      stat += '1
      stat += '1
      stat += '1
      goto merge

right: from statpath
       stat += '3
       stat += '3
       stat += '3
       goto merge

merge: fi dyn from left else right
       goto end
       
dynpath: from init
         stat += dyn
         goto end

end: fi dyn from dynpath else merge
     stat += '3
     exit
