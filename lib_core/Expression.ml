module ISet = Set.Make(Int)

type var = Undef 
         | Def of float

type t = X of int * var
       | Fun of string * t list
       | Const of float

exception Undefined of int
exception Arity of string
let rec eval (e : t) =
  let eval_fun = fun nm vs ->
    match (nm, vs) with
    | ("+",    [v1; v2]) -> v1 +. v2
    | ("-",    [v1; v2]) -> v1 -. v2
    | ("*",    [v1; v2]) -> v1 *. v2
    | ("/",    [v1; v2]) -> v1 /. v2
    | ("pow",  [v1; v2]) -> Float.pow  v1 v2
    | ("exp",  [v])      -> Float.exp  v
    | ("sqr",  [v])      -> v *. v
    | ("sqrt", [v])      -> Float.sqrt v
    | ("cos",  [v])      -> Float.cos  v
    | _ -> raise (Arity (nm ^ " applied to "
                         ^ Int.to_string (List.length vs)
                         ^ " arguments")) in
  match e with
  | X (sub, Undef) -> raise (Undefined sub)
  | X (_, Def f) -> f
  | Fun (nm, es) ->
     let vs = List.map eval es in
     eval_fun nm vs
  | Const f -> f

(* substitute each var in e with its value in x *)
let rec subst (e : t) (x : float Array.t) =
  match e with
  | X (sub, _) ->
     (try
        let value = Array.get x sub
        in X (sub, Def value)
      with Invalid_argument _ -> raise (Undefined sub))
  | Fun (nm, es) ->
     Fun (nm, List.map (fun e -> subst e x) es)
  | Const f -> Const f

let vars (e : t) =
  let rec f = fun e set -> 
    match e with
    | X (sub, _) -> ISet.add sub set
    | Fun (_, es) ->
       List.fold_left (
           fun set e ->
           ISet.union (f e set) set)
         set es
    | Const _ -> ISet.empty in
  f e ISet.empty

let n_vars (e : t) =
  ISet.cardinal (vars e)
