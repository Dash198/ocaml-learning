let list_rev lst = 
  let rec add_eles rls ls = match ls with
  |[] -> rls
  |head::body -> add_eles (head::rls) body in

  add_eles [] lst
