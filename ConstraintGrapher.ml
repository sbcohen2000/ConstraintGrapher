Random.self_init ();;
module ISet = Set.Make(Int)

module Expression =
  struct
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
  end

module Buffer =
  struct
    type elem = Filled of float
              | Empty

    type t = int * elem Array.t

    let create (length : int) =
      (0, Array.make length Empty)

    let add (f : float) ((idx, buff) : t) =
      Array.set buff idx (Filled f);
      (Int.rem (idx + 1) (Array.length buff), buff)

    let mean ((_, buff) : t) =
      let n, sum = Array.fold_left (
                       fun (n, sum) elem ->
                       match elem with Filled f -> (n + 1, sum +. f)
                                     | Empty -> (n, sum))
                     (0, 0.0) buff in
      if n = 0 then Float.infinity
      else sum /. (Float.of_int n)
  end

module type Optimizer = sig
  type syst = Expression.t Array.t
  type vect = float Array.t

  val solve : syst -> vect -> vect
end

module MakeSystem(O : Optimizer) =
  struct
    type syst = Expression.t Array.t (* the system of equations *)
    type vect = float Array.t        (* vector of vars          *)

    (* give a set with the subscript of each x in the system *)
    let vars (a : syst) =
      Array.fold_left (
          fun set expr -> ISet.union (Expression.vars expr) set)
        ISet.empty a

    let n_vars (a : syst) =
      ISet.cardinal (vars a)

    let solve = O.solve
  end

module CoordinateOptimizer =
  struct
    type syst = Expression.t Array.t (* the system of equations *)
    type vect = float Array.t        (* vector of vars          *)

    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> Expression.eval (Expression.subst expr x)) a

    let sqrd_distance (x : vect) (y : vect) =
      let xy = Array.map2 (fun a b -> Float.pow (a -. b) 2.0) x y in
      Array.fold_left (fun sum component -> sum +. component) 0.0 xy

    let slope (x0 : vect) (dim : int) (a : syst) = 
      let epsilon = 0.01 in (* distance to measure slope *)
      let y0 = eval a x0 in (* get a(x0) *)
      let x1 = Array.copy x0 in
      (* move x along dim by epsilon *)
      Array.set x1 dim (Array.get x0 dim +. epsilon);
      print_endline (String.concat "," (Array.to_list (Array.map Float.to_string (Array.concat [x0; y0]))));
      let y1 = eval a x1 in (* get a(x1) *)
      let rise = Array.get y0 dim -. Array.get y1 dim in
      let run = Array.get x0 dim -. Array.get x1 dim in
      rise /. run
    
    let line_search (x : vect) (dim : int) (a : syst) =
      let delta = 0.01 in (* amount to move each iteration *)
      let rec f = fun () ->
        let m = slope x dim a in
        if Float.abs(m) > delta
        then let x_dim = Array.get x dim in
             Array.set x dim (x_dim -. delta *. m); (* should use a proper line search term *)
             f ()
        else x in
      f ()
    
    let solve (a : syst) (x : vect) =
      let delta = ref Float.infinity in
      let curr_x = ref x in
      while !delta > 0.001 do
        let last_x = !curr_x in
        Array.iteri (
            fun i _ ->
            print_endline (Int.to_string i);
            curr_x := line_search !curr_x i a) a;
        delta := sqrd_distance !curr_x last_x;
      done;
      !curr_x
  end

module GradientOptimizer =
  struct
    type syst = Expression.t Array.t (* the system of equations *)
    type vect = float Array.t        (* vector of vars          *)

    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> Expression.eval (Expression.subst expr x)) a

    let objective (a : syst) (x : vect) =
      let vals = Array.map (fun expr -> Expression.eval (Expression. subst expr x)) a in
      Array.fold_left (fun obj x -> obj +. Float.abs x) 0.0 vals

    (* find the local gradient of the system at x *)
    let grad (a : syst) (x : vect) =
      let epsilon = 0.0001 in (* the amount to vary the second point from the first *)
      let x' = Array.map (fun v ->
                   let r = Random.float epsilon in
                   v +. (r -. (epsilon /. 2.))) x in
      let y0 = eval a x  in
      let y1 = eval a x' in
      Array.mapi (fun i a ->
          let rise = (a -. Array.get y1 i) in
          let run = (Array.get x i -. Array.get x' i) in
          rise /. run) y0
    
    (* compute local gradient n times and return the mean *)
    let grad_n (a : syst) (x : vect) (n : int) =
      let len = Array.length x in
      let rec f = fun n ->
        if n = 0
        then Array.make len 0.
        else let x = grad a x in
             let rest = f (n - 1) in
             Array.map2 (fun a b -> a +. b) x rest
      in Array.map (fun v -> v /. (Float.of_int n)) (f n)

    let move (x0 : vect) (x1 : vect) (p : vect) (amount : float) =
      let gj = Array.map2 (fun x y -> amount *. x *. y) x1 p in
      Array.map2 (fun x y -> x -. y) x0 gj
    
    let line_search (a : syst) (x : vect) =
      let m     = 0.05 in  (* maximum move *)
      let alpha = 0.002 in (* amount to decrease each iteration *)
      let g = eval a x in 
      let p = grad_n a x 1000 in (* movement direction *)
      let init_objective = objective a x in
      let x' = ref x in
      let j = ref 0.0 in
      while init_objective <= objective a !x' && Float.abs(!j) < m do
        let amount = m -. !j in
        (* print_endline ("F: " ^ Float.to_string (objective a !x') ^ " alpha: " ^ Float.to_string (amount)); *)
        x' := move x g p amount;
        j := !j +. alpha
      done;
      !x'
    
    let solve (a : syst) (x0 : vect) =
      let rec f = fun x ->
        let x' = line_search a x in
        print_endline (String.concat ", " (Array.to_list (Array.map Float.to_string x)));
        let obj = objective a x' in
        if obj < 0.01 then x
        else f x' in
      f x0
  end

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

module Solver = MakeSystem(GradientOptimizer);;

let z = Solver.solve system [| -2.0; -4.0; |];;

(* Array.iteri (fun i v ->
 *     print_endline ("x" ^ Int.to_string i ^ " <- " ^ Float.to_string v)) z;; *)
