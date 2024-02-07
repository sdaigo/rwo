open Base
open Stdio

let languages = "OCaml,Perl,C++,C"

(* split with ',' then concat with '-' *)
let dashed_language =
  let languages = String.split languages ~on:',' in
  String.concat ~sep:"-" languages

let area_of_ring inner_radius outer_radius =
  let pi = Float.pi in
  let area_of_circle r = pi *. r *. r in
  outer_radius -. area_of_circle inner_radius

let ints, strings = List.unzip [ (1, "one"); (2, "two"); (3, "three") ]

let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

let upcase_first_entry_test =
  equal_string (upcase_first_entry "hello, world!") "HELLO, world!"

let transformed =
  let transforms = [ String.uppercase; String.lowercase ] in
  List.map ~f:(fun g -> g "Hello World") transforms

let plusone x = x + 1
let abs_diff x y = abs (x - y)

(* curry化することで、他の関数をある目的に特化させる *)
let dist_from_3 = abs_diff 3

(* Recursive functions *)
let rec find_first list =
  match list with
  | [] | [ _ ] -> None
  | x :: y :: rest -> if x = y then Some x else find_first (y :: rest)

let found_first_is_5 = find_first [ 1; 2; 3; 4; 5; 5; 6; 7; 8; 8 ]

(* Mutual rec *)
let rec is_even x = if x = 0 then true else is_odd (x - 1)
and is_odd x = if x = 0 then false else is_even (x - 1)

let seven = 3 + 4 (* infix *)
let _seven = 3 + 4 (* prefix *)
let add3 = List.map ~f:(( + ) 3) [ 4; 5; 6 ]
let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin"

let _ =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

let _ =
  let x = 1 in
  x |> fun x -> x + x |> Int.to_string |> print_endline

let _ =
  let x = 1 in
  print_endline @@ Int.to_string @@ (fun x -> x + x) x

(* function keyword *)
let some_or_zero = function Some x -> x | None -> 0

let _ =
  [ Some 1; None; Some 4 ] |> List.map ~f:some_or_zero
  |> List.map ~f:Int.to_string |> List.iter ~f:print_endline
