(a b) -> (a b)

init:
    entry
    a <- ('a . a)
    assert(a && b)
    ('b . b) <- b
    exit
