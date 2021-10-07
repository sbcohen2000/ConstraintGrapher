open Core
open Geometry.Primitives

let ( +@) x y = Expression.Bin (ADD, x, y);;
let ( -@) x y = Expression.Bin (SUB, x, y);;
let ( *@) x y = Expression.Bin (MUL, x, y);;
let ( /@) x y = Expression.Bin (DIV, x, y);;
let f_exp x   = Expression.Mon (EXP, x);;
let f_cos x   = Expression.Mon (COS, x);;
let f_sqr x   = Expression.Mon (SQR, x);;
let f_sqrt x  = Expression.Mon (SQRT, x);;
let const x   = Expression.Const x;;
let x_sub i   = Expression.X i;;


module Solver = System.MakeSystem(System.GradientOptimizer);;

(* f:     sqrt { x_0^2 + x_1 ^2 } - 2
 * fdx_0: a / sqrt { x_0^2 + x_1^2 }
 * fdx_1: b / sqrt { x_0^2 + x_1^2 }
 *) 
let (system : Core.System.eqn array) = [|
    { f = f_sqrt (f_sqr (x_sub 0) +@ f_sqr (x_sub 1)) -@ const 2.0;
      ds = [
          (0, x_sub 0 /@ f_sqrt (f_sqr (x_sub 0) +@ f_sqr (x_sub 1)));
          (1, x_sub 1 /@ f_sqrt (f_sqr (x_sub 0) +@ f_sqr (x_sub 1)));
    ] };
  |]

let soln = ref (Solver.solve system [| 10.0; 10.0; |]);;

let lastfps = ref (Unix.gettimeofday ())
let frames = ref 0
let fps = ref 0.

let update_fps () =
  let t = Unix.gettimeofday () in
  let dt = t -. !lastfps in
  if dt > 0.5 then (
    fps := float !frames /. dt;
    frames := 0;
    lastfps := t
  );
  incr frames

let clamp (min : float) (max : float) (f : float) =
  if f < min then min
  else if f > max then max
  else f

let color_to_int (r, g, b : float * float * float) =
  let c = clamp 0.0 1.0 in
  let to_int32 = fun v -> Int32.of_int (Float.to_int (c v *. 255.0)) in
  let r_int = to_int32 r in
  let g_int = to_int32 g in
  let b_int = to_int32 b in
  List.fold_left (fun n elem ->
      Int32.logor n elem) Int32.zero
    [ Int32.shift_left r_int 16;
      Int32.shift_left g_int 8;
      Int32.shift_left b_int 0 ]

let x_scale = 10.0;;
let y_scale = 10.0;;

let screenspace_to_coord (width : int) (height : int) (x : int) (y : int) =
  let x_extent = Float.of_int x /. Float.of_int width in
  let y_extent = Float.of_int y /. Float.of_int height in
  let x_coord = x_extent *. 2. *. x_scale -. x_scale in
  let y_coord = y_extent *. 2. *. y_scale -. y_scale in
  x_coord, y_coord;;

let coord_to_screenspace (width : int) (height : int) (x_0 : float) (x_1 : float) =
  let x_extent = (x_0 +. x_scale) /. (2. *. x_scale) in
  let y_extent = (x_1 +. y_scale) /. (2. *. y_scale) in
  let x = Float.to_int (x_extent *. Float.of_int width) in
  let y = Float.to_int (y_extent *. Float.of_int height) in
  x, y;;

let max_obj = ref 0.0;;

(* maps a value on [0, 1] to a color *)
let val_to_color (v : float) = 
  let b = Float.log10 (1000000. *. v +. 1.) in
  let g = Float.log10 (1000. *. v +. 1.) in
  let r = Float.log10 (100. *. v +. 1.) in
  let v = (r +. g +. b) /. 3. in
  Int32.to_int (color_to_int (v, v, v))

let render_background sx sy =
  Array.init sy
    (fun y ->
      Array.init sx
        (fun x ->
          let x_0, x_1 = screenspace_to_coord sx sy x y in
          let obj = Solver.objective system [| x_0; x_1 |] in
          if obj > !max_obj then max_obj := obj;
          let value = obj /. !max_obj in
          val_to_color value
    ))

let expose () =
  let sx = Graphics.size_x () in
  let sy = Graphics.size_y () in
  let mx, my = Graphics.mouse_pos () in
  let tx, ty = screenspace_to_coord sx sy mx my in
  let ax, ay = Array.get !soln 0, Array.get !soln 1 in
  let dx, dy = Point.norm (Point.sub (ax, ay) (tx, ty)) in
  soln := Solver.vary_solution system !soln (0, 1) (dx, dy);
  let data_img = render_background sx sy in
  Graphics.draw_image (Graphics.make_image data_img) 0 0;
  let x_0 = Array.get !soln 0 in
  let x_1 = Array.get !soln 1 in
  let cx, cy = coord_to_screenspace sx sy x_0 x_1 in
  Graphics.set_color Graphics.blue;
  Graphics.draw_circle cx cy 4;
  Graphics.synchronize ();
  (* Update our fps counter. *)
  update_fps ()

let () =
  Graphics.open_graph "";
  Graphics.auto_synchronize false;
  while true do
    expose ();
  done
