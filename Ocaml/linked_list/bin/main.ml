type node = {data: int; pointer: node option}

let make_ll lst = 
  let rec add_nodes lst = match lst with
  |head::body -> {data = head; pointer = (if body = [] then None else Some (add_nodes body))} in

  add_nodes lst

let print_ll llst =
  let rec print_node ls = match ls with
  {data;pointer} -> Printf.printf "%d " data;
  match pointer with
  |None -> Printf.printf "\n";
  |Some node -> print_node node; in

  print_node llst

let rec del_node lst value =
  match lst with
  |{data;pointer} -> if data = value then pointer
  else Some {data = data; pointer = (match pointer with |None -> None |Some node -> (del_node node value ))}


let () = print_endline "Hello, World!"
