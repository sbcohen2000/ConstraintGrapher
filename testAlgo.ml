open Core

let ( +@) x y = Expression.Bin (ADD, x, y);;
let ( -@) x y = Expression.Bin (SUB, x, y);;
let ( *@) x y = Expression.Bin (MUL, x, y);;
let ( /@) x y = Expression.Bin (DIV, x, y);;
let f_exp x   = Expression.Mon (EXP, x);;
let f_cos x   = Expression.Mon (COS, x);;
let f_sqr x   = Expression.Mon (SQR, x);;
let const x   = Expression.Const x;;
let x_sub i   = Expression.X i;;

let system = [|
    f_sqr (x_sub 0) -@ x_sub 1;
    x_sub 1 -@ const 4.0 |]

module Solver = System.MakeSystem(System.DirectOptimizer);;

let z = Solver.solve system [| 2.124; 2.934; |];;

Array.iteri (fun i v ->
    print_endline ("x" ^ Int.to_string i ^ " <- " ^ Float.to_string v)) z;;
