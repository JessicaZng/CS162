fun take with n, l =
    if ((isnil l) || (n < 1)) then Nil
    else (!l) @ ((take (n-1)) #l)
in
take