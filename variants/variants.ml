open Base
open Stdio

(* ターミナルの基本色 *)
type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

(* 基本色を整数コードに変換する *)
let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7

let _ = basic_color_to_int Green |> printf "color: %d\n"

(* 文字列を指定した基本色で表示するためのエスケープされたコードを返す *)
let color_by_number number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

let red_text = color_by_number (basic_color_to_int Red) "Hello Red Text!"
let _ = print_endline red_text
let blue_text = color_by_number (basic_color_to_int Blue) "Blue"
let _ = printf "Hello %s Text!\n" blue_text

type weight = Regular | Bold

(* type color =
   | Basic of basic_color * weight
   | RGB of int * int * int
   | Gray of int *)

type color =
  | Basic of basic_color (* basic colors *)
  | Bold of basic_color (* bold basic colors *)
  | RGB of int * int * int (* 6x6x6 color cube *)
  | Gray of int (* 24 grayscale levels *)

(* 色を対応する数値に変換する *)
let color_to_int = function
  | Basic basic_color -> basic_color_to_int basic_color
  | Bold basic_color -> 8 + basic_color_to_int basic_color
  | RGB (r, g, b) -> 16 + b + (g * 6) + (r * 36)
  | Gray i -> 232 + i

(* 色情報を加えて出力する *)
let color_print color s = printf "%s\n" (color_by_number (color_to_int color) s)

type figure = Point | Circle of int | Rectangle of int * int | Square of int
