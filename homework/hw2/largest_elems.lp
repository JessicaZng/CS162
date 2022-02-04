fun max with a,b =
  if (a > b) then a
  else b
in
fun largest with l =
  if (isnil l) then Nil //
  else if (isnil (#l)) then (!l)
  else ((max !l) (largest (#l)))
in
fun largest_elems with l =
  if (isnil l) then Nil
  else if (isnil (!l)) then (largest_elems (#l))
  else ((largest (!l)) @ (largest_elems (#l)))
in
largest_elems