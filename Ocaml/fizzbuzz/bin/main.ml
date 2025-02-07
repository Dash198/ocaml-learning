let rec fizz_buzz n i =
  if i = n+1 then ""
  else
    (let result = (if i mod 3 = 0 then "Fizz" else "") ^ (if i mod 5 = 0 then "Buzz" else "") in
    if result = "" then (string_of_int i) else result)^"\n"^ (fizz_buzz n (i+1))

let () =
    Printf.printf "Enter n: ";
    let n = read_int () in
    Printf.printf "%s\n" (fizz_buzz n 1)
