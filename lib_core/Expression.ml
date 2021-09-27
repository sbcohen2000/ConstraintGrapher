module ISet = Set.Make(Int)

type var = Undef 
         | Def of float

type bin_op = ADD
            | SUB
            | MUL
            | DIV
            | POW

type mon_op = EXP
            | SQR
            | SQRT
            | COS

type t = X of int
       | Bin of bin_op * t * t
       | Mon of mon_op * t
       | Const of float

exception Undefined of int
exception Arity of string
let rec eval (e : t) =
  let eval_bin = fun op va vb ->
    match op with
    | ADD -> va +. vb
    | SUB -> va -. vb
    | MUL -> va *. vb
    | DIV -> va /. vb
    | POW -> Float.pow va vb in

  let eval_mon = fun op v ->
    match op with
    | EXP -> Float.exp v
    | SQR -> v *. v
    | SQRT -> Float.sqrt v
    | COS -> Float.cos v in

  match e with
  | X sub -> raise (Undefined sub)
  | Bin (op, ea, eb) ->
     let va = eval ea in
     let vb = eval eb in
     eval_bin op va vb
  | Mon (op, e) ->
     let v = eval e in
     eval_mon op v
  | Const f -> f

(* substitute each var in e with its value in x *)
let rec subst (e : t) (x : float Array.t) =
  match e with
  | X sub ->
     (try
        let value = Array.get x sub
        in Const value
      with Invalid_argument _ -> raise (Undefined sub))
  | Bin (op, ea, eb) ->
     Bin (op, subst ea x, subst eb x)
  | Mon (op, e) ->
     Mon (op, subst e x)
  | Const f -> Const f

let vars (e : t) =
  let rec f = fun e set -> 
    match e with
    | X sub -> ISet.add sub set
    | Bin (_, ea, eb) ->
       ISet.union (ISet.union (f ea set) (f eb set)) set
    | Mon (_, e) ->
       ISet.union (f e set) set
    | Const _ -> ISet.empty in
  f e ISet.empty

let n_vars (e : t) =
  ISet.cardinal (vars e)

let bin_op_to_string (op : bin_op) =
  match op with
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | POW -> "**"

let mon_op_to_string (op : mon_op) =
  match op with
  | EXP -> "exp"
  | SQR -> "sqr"
  | SQRT -> "sqrt"
  | COS -> "cos"

let rec to_string (e : t) =
  match e with
  | X sub -> "x_" ^ Int.to_string sub
  | Bin (op, a, b) -> to_string a ^ " " ^ bin_op_to_string op ^ to_string b
  | Mon (op, a) -> mon_op_to_string op ^ "(" ^ to_string a ^ ")"
  | Const f -> Float.to_string f
