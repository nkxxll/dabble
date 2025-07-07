open! Core

let input =
  "(A, B, 0)\n\
   (A, C, 1)\n\
   (B, *D, 0)\n\
   (B, *E, 1)\n\
   (C, *D, 0)\n\
   (C, *E, 1)\n\
   (*D, *D, 0)\n\
   (*D, *D, 1)\n\
   (*E, *E, 0)\n\
   (*E, *E, 1)"
;;

module Line = struct
  open Core

  type node_type =
    { letter : char
    ; end_ : bool
    }
  [@@deriving show]

  type zustand =
    | Open
    | Node of node_type
    | AlphaLetter of char
    | Close
  [@@deriving show]

  let parse line =
    let tokens =
      String.to_list line
      |> List.filter ~f:(fun c ->
        not (Char.is_whitespace c || Char.equal c ','))
    in
    let parse_node chars =
      match chars with
      | '*' :: c :: tl when Char.(c >= 'A' && c <= 'Z') ->
        { letter = c; end_ = true }, tl
      | c :: tl when Char.(c >= 'A' && c <= 'Z') ->
        { letter = c; end_ = false }, tl
      | _ -> failwith "Invalid node format"
    in
    let rec loop acc tokens =
      match tokens with
      | [] -> List.rev acc
      | '(' :: rest -> loop (Open :: acc) rest
      | ')' :: rest -> loop (Close :: acc) rest
      | _ ->
        let from_node, rest1 = parse_node tokens in
        let to_node, rest2 = parse_node rest1 in
        (match rest2 with
         | digit :: tail when Char.is_digit digit ->
           loop
             (AlphaLetter digit :: Node to_node :: Node from_node :: acc)
             tail
         | _ -> failwith "Expected digit after nodes")
    in
    loop [] tokens
  ;;
end

module Pair = struct
  module T = struct
    type t = char * char [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse_lines lines =
  let transitions = ref Pair.Map.empty in
  let unique_pairs = ref Pair.Map.empty in
  List.iter lines ~f:(fun line ->
    let parsed = Line.parse line in
    match parsed with
    | [ Line.Open
      ; Line.Node from_node
      ; Line.Node to_node
      ; Line.AlphaLetter c
      ; Line.Close
      ] ->
      let key = from_node.letter, to_node.letter in
      transitions := Map.set !transitions ~key ~data:c;
      unique_pairs
      := Map.set !unique_pairs ~key ~data:(from_node.end_ || to_node.end_)
    | _ -> Printf.eprintf "Invalid line format: %s\n" line);
  !transitions, !unique_pairs
;;

(* I have key pair x y and want the transitions from x y with 0 and with 1
   so
   filter: start with x or start with y
   take: start with x val 1 start with y val 1
   take: start with x val 0 start with y val 0
   return: pair for 1 and pair for 2
   *)
let get_pair key transitions =
  let k_f, k_s = key in
  let transitions = Map.filteri transitions ~f:(fun ~key ~data:_ -> (
    let first, _ = key in
    (Char.equal first k_f || Char.equal first k_s)
)) |> Map.to_alist in
  let rec loop aux1 aux2 rest =
match rest with 
  | [] -> aux1, aux2
  | ((_, second), value)::rest when Char.equal value '0' -> loop (second::aux1) aux2 rest
  | ((_, second), value)::rest when Char.equal value '1' -> loop aux1 (second::aux2) rest
      |_-> failwith "should not happen"
  in loop [] [] transitions;;


(* let readlines = In_channel.input_lines In_channel.stdin *)
let algo unique_pairs transitions =
  Map.mapi unique_pairs ~f:(fun ~key ~data ->
  if not data then
  match get_pair key transitions with
  | [ffirst; fsecond], [sfirst, ssecond] -> (
  let fkey = (ffirst, fsecond) in
let skey = (sfirst, ssecond) in
        let ffind = Map.find unique_pairs fkey in 
        let sfind = Map.find unique_pairs skey in
)
  | failwith "no dea cannot happen"
  )

  else data

let () =
  (* let lines = readlines in *)
  Printf.printf "\n";
  let lines = String.split_lines input in
  let transitions, unique_pairs = parse_lines lines in
  let unique_pairs = algo unique_pairs transitions in
  printf "Transitions:\n";
  Map.iteri transitions ~f:(fun ~key:(from_, to_) ~data ->
    printf "%c -> %c via '%c'\n" from_ to_ data);
  printf "\nAll Unique Pairs:\n";
  Map.iteri unique_pairs ~f:(fun ~key:(from_, to_) ~data:d ->
    printf "%c <-> %c: connected %b\n" from_ to_ d)
;;
