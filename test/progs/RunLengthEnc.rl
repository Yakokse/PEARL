// Run-length encoding using stacks

//procedure encode(stack text, stack arc)
//    from !empty(text) && empty(arc) do
//        local int val = 0
//        local int n = 0
//        val += top(text)
//        from n = 0 do
//            local int tmp = 0
//            pop(tmp, text)
//            delocal int tmp = val
//            n += 1
//        until empty(text) || top(text) != val
//        push(val, arc)
//        push(n, arc)
//        delocal int n = 0
//        delocal int val = 0
//    until empty(text)

init: entry
      if input = 'nil goto stop else loop

loop1: fi res = 'nil from init else loop
       count ^= 0
       
       if input = 'nil goto stop else loop

stop: fi res = 'nil from init else loop
      exit