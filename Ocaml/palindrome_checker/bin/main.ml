let rec reverse_string s = 
  if String.length s = 1 then s
  else (String.make 1 (String.get s (String.length s -1)) ^ (reverse_string (String.sub s 0 (String.length s-1))))

let palindrome_check str = 
  if str = reverse_string str then "String is Palindrome"
  else "String is not Palindrome"

let () = 
  print_endline "Enter a string:";
  let str = read_line () in

  Printf.printf "%s\n" (palindrome_check str)
