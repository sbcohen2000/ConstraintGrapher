Random.self_init ()
module ISet = Set.Make(Int)

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

    let to_string (a : syst) =
      String.concat "; " (Array.to_list (Array.map Expression.to_string a))
    
    let solve = O.solve
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
