open Base
open Stdio

let square x = x * x
let ratio x y = Float.of_int x /. Float.of_int y

let ratio' x y =
  let open Base.Float.O in
  of_int x / of_int y

(* 関数を引数にとる関数 *)
let sum_if_true test first second =
  (if test first then first else 0) + if test second then second else 0

(* 型推論 *)
(* 1. if は両方の枝が同じ型である必要があるので、first と 0 は同じ int 型である *)
(* 2. 同様に second も 0 と同じ int 型 *)
(* 3. test, first は引数として渡されるので、first が int なので、test の入力は int である *)
(* 4. test, first if 式の条件として使用されるので test の返り値は bool 型である *)
(* 5. + が int を返すので、 sum_if_true の返り値も int 型である *)

let even x = x % 2 = 0
let () = sum_if_true even 3 4 |> Int.to_string |> print_endline (* 4 *)
let () = sum_if_true even 2 4 |> Int.to_string |> print_endline (* 6 *)

(* 'a はジェネリックな型を表す *)
(* ('a -> bool) -> 'a -> 'a -> 'a *)
let first_if_true test x y = if test x then x else y
