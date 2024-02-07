open Base
open Stdio

type point2d = { x : float; y : float }

let p = { x = 3.; y = -4. }
let magnitude { x; y } = Float.sqrt ((x **. 2.) +. (y **. 2.))

(* bind the variable x_pos to the value of x *)
let magnitude' { x = pos_x; y = pos_y } =
  Float.sqrt ((pos_x **. 2.) +. (pos_y **. 2.))

let distance p1 p2 = magnitude { x = p1.x -. p2.x; y = p1.y -. p2.y }

(* include record types as components in larger types. *)
type circle_desc = { center : point2d; radius : float }
type rect_desc = { lower_left : point2d; width : float; height : float }
type segment_desc = { endpoint1 : point2d; endpoint2 : point2d }

(* define these objects together in a single type *)
type scene_element =
  | Circle of circle_desc
  | Rect of rect_desc
  | Segment of segment_desc

(* ある point が、scene_elementsのリストのある要素の内部にあるかどうかをテストする *)
let is_inside_scene_element point scene_element =
  match scene_element with
  | Circle { center; radius } ->
      (* 中心からの距離が半径以内である *)
      Float.( > ) (distance center point) radius
  | Rect { lower_left; width; height } ->
      (* p.x が左下の点から幅を足した右下までの距離にある *)
      (* かつ p.y が左下の点から高さを足した左上までの距離にある *)
      Float.( > ) point.x lower_left.x
      && Float.( < ) point.x (lower_left.x +. width)
      && Float.( > ) point.y lower_left.y
      && Float.( < ) point.y (lower_left.y +. height)
  | Segment _ -> false

let () =
  let result =
    is_inside_scene_element { x = 3.; y = 7. }
      (Circle { center = { x = 4.; y = 4. }; radius = 0.5 })
  in
  printf "point(3.0, 7.0) is in Circle(4.0, 4.0, r=0.5) %b\n" result

let () =
  let result' =
    is_inside_scene_element { x = 3.; y = 7. }
      (Circle { center = { x = 4.; y = 4. }; radius = 5. })
  in
  printf "point(3.0, 7.0) is in Circle(4.0, 4.0, r=5.0) %b\n" result'
