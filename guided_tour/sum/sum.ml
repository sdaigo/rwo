open Base
open Stdio

(* 入力から数値を受け取り、合計を出力する *)
let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () = printf "quit with CTRL+D"
let () = printf "Total: %f\n" (read_and_accumulate 0.0)
