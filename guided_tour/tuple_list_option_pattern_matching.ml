open Base
open Stdio

(* Tuple *)
let a_tuple = (3, "three")
let b_tuple = (4, "four")

(* extract the components *)
let () =
  let x, y = a_tuple in
  printf "x = %d, y = \"%s\"\n" x y

let distance (x1, y1) (x2, y2) =
  Float.sqrt (((x1 -. x2) **. 2.) +. ((y1 -. y2) **. 2.))

(* List *)
let languages = [ "OCaml"; "Perl"; "C" ]
let () = printf "%d languages!\n" (List.length languages)
let counts_in_languages = List.map languages ~f:String.length

(* Baseでは map に渡す関数はラベル付き *)
let counts_in_languages' = List.map ~f:String.length languages
let languages' = "French" :: "Spanish" :: languages
let my_favorite_languages (my_favorite :: _rest) = my_favorite
let my = my_favorite_languages languages'

let my_favorite_language' languages =
  match languages with first :: _ -> first | [] -> "OCaml" (* default! *)

let my' = my_favorite_language' languages
let rec sum lst = match lst with [] -> 0 | hd :: tl -> hd + sum tl
let total = sum [ 1; 2; 3 ]

let rec remove_sequential_duplicates list =
  match list with
  | [] -> []
  | [ x ] -> [ x ]
  | first :: second :: tl ->
      if first = second then remove_sequential_duplicates (second :: tl)
      else first :: remove_sequential_duplicates (second :: tl)

let normalized = remove_sequential_duplicates [ 1; 1; 2; 3; 3; 4; 4; 1; 1; 1 ]
(* [1;2;3;4;1] *)

(* Option *)
let divide x y = if y = 0 then None else Some (x / y)

(* 拡張子付きのファイル名を受け取り、小文字に直す *)
let downcase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext

let normalized_files =
  List.map ~f:downcase_extension
    [ "Hello_World.TXT"; "Hello_World.txt"; "Hello_World" ]
(* ["Hello_World.txt"; "Hello_World.txt"; "Hello_World"] *)
