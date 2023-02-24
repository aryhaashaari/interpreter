(*Writing a line to a file*)
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp
;;
(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)

type command = Push | Pop | Quit | Add | Sub | Mul | Div | Swap | Neg | Concat | And | Or | Not | Equal | Lte | Local | Global | Begin | End | IfThen | Else
let command_of_string = function
| "Push" -> Push
| "Pop" -> Pop
| "Quit" -> Quit
| "Add" -> Add
| "Sub" -> Sub
| "Mul" -> Mul
| "Div" -> Div
| "Swap" -> Swap
| "Neg" -> Neg
| "Concat" -> Concat
| "And" -> And
| "Or" -> Or
| "Not" -> Not
| "Equal" -> Equal
| "Lte" -> Lte
| "Local" -> Local
| "Global" -> Global
| "Begin" -> Begin
| "End" -> End
| "IfThen" -> IfThen
| "Else" -> Else
| _ -> raise (Invalid_argument "\"Error\"")

let string_splitter x = 
    String.split_on_char ' ' x
    |> List.filter (fun s -> s <> "")
;;

let str_to_comm x = 
  command_of_string (List.nth (string_splitter x) 0)
;;

let fin_split x=
  let split x =
  String.split_on_char '\n' x
  |> List.filter (fun s -> s <> "")
in List.fold_right(fun x y -> (String.trim x)::y) (split x) []
;;



let op_check x =
  if (Str.string_match (Str.regexp "[0-9]+$") (x) 0) then
    true
  else (if (String.sub x 0 1) = "-" then
    (if (Str.string_match (Str.regexp "[0-9]+$") (x) 1) then
      true
    else 
      false)
    else
      false)
  ;;

let div_check x =
    if (x = "0") then
      false
    else 
      true
;;

let push_check x =
  if Char.equal(String.get (x) 0) '\"'  then 
    (if String.length (x) = 2 then
      true 
    else if (Str.string_match (Str.regexp "[a-zA-Z]+$") (Str.string_before (x) (String.length (x)-1)) 1) then 
      true 
    else false)
  else (if op_check x then 
    true
      else 
    false) 
;;
let rec print_numbers oc = function 
  | [] -> ()
  | e::tl -> Printf.fprintf oc "%s\n" e; print_numbers oc tl

let file_print  file_path nums =
  let oc = open_out file_path in
  print_numbers oc nums;
  close_out oc;
;;

let concat a b =
  "\""^(Str.string_after (Str.string_before a ((String.length a)-1)) 1)^(Str.string_after (Str.string_before b ((String.length b)-1)) 1)^"\""
;;

let boolcheck a =
  if (String.length a == 1) && (Str.string_match (Str.regexp "[0-1]") (a) 0) then
    true
  else
    false
;;

let and_result a b =
  if a == 1 && b == 1 then
    "1"
  else 
    "0"
;;

let or_result a b =
  if a == 0 && b == 0 then
    "0"
  else 
    "1"
;;

let not_result a =
  if a == 0 then
    "1"
  else 
    "0"
;;

let eq_result a b =
  if int_of_string(a) == int_of_string(b) then
    "1"
  else 
    "0"
;;

let lte_result a b =
  if int_of_string(a) <= int_of_string(b) then
    "1"
  else 
    "0"
;;

let name_check a = 
  if (Str.string_match (Str.regexp "[a-z]+[a-zA-Z0-9_]*") a 0) then
    true 
  else 
    false
;;

let rec first_index a =
  match a with
  | [] -> []
  | h::t -> (List.nth (string_splitter h) 0):: first_index t
;;

let rec find_var a b=
  match a with
  | [] -> "Hello"
  | h::t -> 
    (if (String.sub (h) (0) ((String.length b))) = b then
      h
    else 
      find_var t b
    )
;;

let interpreter (src : string) (output_file_path: string): unit = 
  let rec continue src_lst acc localacc globalacc tempacc templocalacc check been=
    match src_lst with 
    | [] -> []
    | h::t ->
      match str_to_comm h with
      | Push -> 
        ( if check then
        (if(push_check (List.nth (string_splitter h) 1)) then 
          continue t ((List.nth (string_splitter h) 1)::acc) localacc globalacc tempacc templocalacc check been
        else (if List.mem (List.nth (string_splitter h) 1) (first_index localacc) then
          continue t ((List.nth (string_splitter (find_var localacc (List.nth (string_splitter h) 1))) 1)::acc) localacc globalacc tempacc templocalacc check been
        else (if List.mem (List.nth (string_splitter h) 1) (first_index globalacc) then
          continue t ((List.nth (string_splitter (find_var globalacc (List.nth (string_splitter h) 1))) 1)::acc) localacc globalacc tempacc templocalacc check been
        else
          continue ["Quit"] ["\"Error\""] [] [] [] [] check been)
          )
        ) 
        else continue t acc localacc globalacc tempacc templocalacc check been
        )

      | Pop -> 
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | hd::tl -> continue t tl localacc globalacc tempacc templocalacc check been
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Quit -> 
        (if check then acc
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Add ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if(op_check a) && (op_check b) then
            continue t ((string_of_int(int_of_string(a) + int_of_string(b)))::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Sub ->
        (if check then
        (match acc with 
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if(op_check a) && (op_check b) then
            continue t (string_of_int(int_of_string(a) - int_of_string(b))::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Mul -> 
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if(op_check a) && (op_check b) then
            continue t (string_of_int(int_of_string(a) * int_of_string(b))::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Div ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if(op_check a) && (op_check b) && (div_check b) then
            continue t (string_of_int(int_of_string(a) / int_of_string(b))::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Swap ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _::[] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> continue t (b::a::c) localacc globalacc tempacc templocalacc check been
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Neg ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b -> 
          (if op_check a then
            continue t (string_of_int(-1* (int_of_string(a)))::b) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Concat ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if(op_check a) || (op_check b) then
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          else
            continue t ((concat a b)::c) localacc globalacc tempacc templocalacc check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | And ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _::[] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if boolcheck a && boolcheck b then
            continue t (and_result (int_of_string(a)) (int_of_string(b))::c) localacc globalacc tempacc templocalacc check been
          else 
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Or ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _::[] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c -> 
          (if boolcheck a && boolcheck b then
            continue t (or_result (int_of_string(a)) (int_of_string(b))::c) localacc globalacc tempacc templocalacc check been
          else 
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Not ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b ->
          (if boolcheck a then
            continue t (not_result(int_of_string(a))::b) localacc globalacc tempacc templocalacc check been
          else 
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
            )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Equal ->
        (if check then
        (match acc with 
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c ->
          (if (op_check a) && (op_check b) then
            continue t ((eq_result a b)::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
            )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Lte ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | _ :: [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b::c ->
          (if (op_check a) && (op_check b) then
            continue t ((lte_result a b)::c) localacc globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
            )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Local -> 
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b -> 
          (if name_check (List.nth (string_splitter h) 1) then
            continue t b (((List.nth (string_splitter h) 1)^" "^a)::localacc) globalacc tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          ) 
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Global ->
        (if check then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b -> 
          (if name_check (List.nth (string_splitter h) 1) then
            continue t b localacc (((List.nth (string_splitter h) 1)^" "^a)::globalacc) tempacc templocalacc check been
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
          )
        )
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | Begin -> (if check then
        continue t [] localacc globalacc (List.append acc tempacc) (List.append localacc templocalacc) check been
        else continue t acc localacc globalacc tempacc templocalacc check been
        )
      | End -> 
        (match acc with 
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b ->
          (if been then
            continue t acc localacc globalacc tempacc templocalacc true false
          else
            continue t (a::tempacc) templocalacc globalacc [] [] true been)
        )
      | IfThen -> 
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b -> 
          (if boolcheck a then 
            (if int_of_string(a) == 1 then
              continue t b localacc globalacc tempacc templocalacc true true
            else
              continue t acc localacc globalacc tempacc templocalacc false false)
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
            )
        )
      | Else ->
        (if been == false then
        (match acc with
        | [] -> continue ["Quit"] ["\"Error\""] [] [] [] [] check been
        | a::b -> 
          (if boolcheck a then 
            (if int_of_string(a) == 0 then
              continue t b localacc globalacc tempacc (List.append localacc templocalacc) true true
            else
              continue t acc localacc globalacc tempacc templocalacc false false)
          else
            continue ["Quit"] ["\"Error\""] [] [] [] [] check been
            )
        )
          else continue t acc localacc globalacc tempacc templocalacc false false)
  in file_print output_file_path (continue (fin_split src) [] [] [] [] [] true false);;
