// H_n(a,b) as specified in https://en.wikipedia.org/wiki/Hyperoperation
// Implemented as a TRS

init: entry
      assert(stack = 'nil)
      stack <- (stack)

calc: from genstack

stop: from init
      exit