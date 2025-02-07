let add x y = x+.y
let subtract x y = x-.y
let multiply x y = x*.y
let divide x y = 
  if y=0. then raise(Failure "Cannot divide by zero")
  else x/.y

let calculate x op y = match op with
|"+" -> add x y
|"-" -> subtract x y
|"*" -> multiply x y
|"/" -> divide x y
|_ -> raise(Failure "Invalid Operator")

let () = 
  print_endline "Enter first number: ";
  let x = read_float () in

  print_endline "Enter operator (+,-,*,/): ";
  let op = read_line () in

  print_endline "Enter second number: ";
  let y = read_float () in

  let result = calculate x op y in
  Printf.printf "Result: %f\n" result 