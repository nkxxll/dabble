(** this is a dabble to recreate the r algo which is to get a regular expression from a dea
    the algo works like this the regular exrepssion is generated recursively we start with three params k i j
    k is the highest node over which the regex can "go" i is the start and j is the end
    the algo is defined like this r k i j = r k-1 i j + r k-1 i k (r k-1 k k)* r k-1 k j

    1. the mvp for me is to get the right number of formulas that I have to calc
    2. next step is to let it calculate the full regex (without simplification)
    3. next step is to remove the empty lists and expressions that are connected to a empty list
*)
open! Core

module R = struct
  type t =
    { k : int
    ; i : int
    ; j : int
    }
  [@@deriving compare, sexp, show]
end

module RSet = Set.Make (R)

let cache : RSet.t ref = ref RSet.empty
let lines = "R 4 1 4"

let parse_r line =
  let split = String.split line ~on:' ' in
  let k = Int.of_string (List.nth_exn split 1) in
  let i = Int.of_string (List.nth_exn split 2) in
  let j = Int.of_string (List.nth_exn split 3) in
  { R.k; i; j }
;;

let rec gen_recurse r =
  if Int.equal r.R.k 0
  then cache := Set.add !cache r
  else (
    let fr = { R.k = r.k - 1; i = r.i; j = r.j } in
    cache := Set.add !cache fr;
    let sr = { R.k = r.k - 1; i = r.i; j = r.k } in
    cache := Set.add !cache sr;
    let tr = { R.k = r.k - 1; i = r.k; j = r.k } in
    cache := Set.add !cache tr;
    let lr = { R.k = r.k - 1; i = r.k; j = r.j } in
    cache := Set.add !cache lr;
    gen_recurse fr;
    gen_recurse sr;
    gen_recurse tr;
    gen_recurse lr)
;;

let () =
  (* let readlines = In_channel.input_lines In_channel.stdin *)
  let readlines = [ lines ] in
  readlines
  |> List.map ~f:parse_r
  |> List.iter ~f:(fun r ->
    cache := Set.add !cache r;
    gen_recurse r);
  printf "Generated %d records:\n" (Set.length !cache + 1);
  Set.to_list !cache
  |> List.sort ~compare:(fun a b ->
    if not (Int.equal a.R.k b.R.k)
    then b.R.k - a.R.k
    else if not (Int.equal a.R.i b.R.i)
    then b.R.i - a.R.i
    else if not (Int.equal a.R.j b.R.j)
    then b.R.i - a.R.i
    else 0)
  |> List.iter ~f:(fun r -> printf "%s\n" (R.show r))
;;
