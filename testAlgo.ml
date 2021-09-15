open Core

let ( +@) x y = Expression.Fun ("+", [x; y]);;
let ( -@) x y = Expression.Fun ("-", [x; y]);;
let ( *@) x y = Expression.Fun ("*", [x; y]);;
let ( /@) x y = Expression.Fun ("/", [x; y]);;
let f_exp x   = Expression.Fun ("exp", [x]);;
let f_cos x   = Expression.Fun ("cos", [x]);;
let f_sqr x   = Expression.Fun ("sqr", [x]);;
let const x   = Expression.Const x;;
let x_sub i   = Expression.X (i, Expression.Undef);;

(* let system = [|
 *     ((const 3.) *@ (x_sub 0)) -@ (f_cos (x_sub 1 *@ x_sub 2)) -@ const (3. /. 2.);
 *     ((const 4.) *@ f_sqr (x_sub 0)) -@ ((const 625.) *@ f_sqr (x_sub 1)) +@ ((const 2.) *@ (x_sub 1)) -@ (const 1.);
 *     (f_exp (x_sub 0) *@ (x_sub 1) *@ (const (-1.))) +@ ((const 20.) *@ (x_sub 2)) +@ (const 9.472) |];; *)

let system = [|
    f_sqr (x_sub 0) -@ x_sub 1;
    x_sub 1 -@ const 4.0 |]

module Solver = System.MakeSystem(System.GradientOptimizer);;

let z = Solver.solve system [| -2.0; -4.0; |];;

Array.iteri (fun i v ->
    print_endline ("x" ^ Int.to_string i ^ " <- " ^ Float.to_string v)) z;;
