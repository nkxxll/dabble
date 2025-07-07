open! Core

module Line = struct
  type zustand =
    | Open
    | Node of char
    | AlphaLetter of char
    | WhiteSpace
    | Close

  let convert_char char =
    match char with
    | '(' -> Open
    | 'A' .. 'Z' | 'a' .. 'z' -> Node char
    | '0' .. '9' -> AlphaLetter char
    | ')' -> Close
    | ',' | ' ' | '\t' | '\n' -> WhiteSpace
    | _ -> failwith (Printf.sprintf "Could not parse char %c" char)
  ;;

  let is_node (zu : zustand) =
    match zu with
    | Node _ -> true
    | _ -> false
  ;;

  let pair_nodes list =
    let rec loop new_ old =
      match old with
      | [] -> new_
      | x :: y :: rest when is_node x && is_node y ->
        loop ([ x; y ] :: new_) rest
      | something :: rest -> loop ([ something ] :: new_) rest
    in
    loop [] list
  ;;

  (* the list is a but bad but it is how it is this is healing coding for me *)
  let rec check last list =
    match last, list with
    | [], [ Open ] :: rest -> check [ Open ] rest
    | [ Close ], [] -> true
    | [ Close ], [ Open ] :: rest -> check [ Open ] rest
    | [ Open ], [ Node x; Node y ] :: rest -> check [ Node x; Node y ] rest
    | [ Node _; Node _ ], [ AlphaLetter z ] :: rest ->
      check [ AlphaLetter z ] rest
    | [ AlphaLetter _ ], [ Close ] :: rest -> check [ Close ] rest
    | _ -> false
  ;;

  let parse line =
    let parsed =
      String.to_list line |> List.map ~f:convert_char |> pair_nodes
    in
    let out = if check [] parsed then "checked" else "false" in
    Printf.printf "%s\n" out
  ;;
end

let readlines = In_channel.input_lines In_channel.stdin

let () =
  let lines = readlines in
  let _ = List.map lines ~f:Line.parse in
  ()
;;
