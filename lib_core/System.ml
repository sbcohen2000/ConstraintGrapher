Random.self_init ()
module ISet = Set.Make(Int)

module type Optimizer = sig
  type syst = Expression.t Array.t
  type vect = float Array.t

  (* call solve with a custom parameter *)
  val vary_solution : syst -> vect -> int -> float -> vect
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

    let vary_solution = O.vary_solution
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
        (* print_endline (String.concat ", " (Array.to_list (Array.map Float.to_string x))); *)
        let obj = objective a x' in
        if obj < 0.01 then x
        else f x' in
      f x0
  end

module DirectOptimizer =
  struct
    type syst = Expression.t Array.t (* the system of equations *)
    type vect = float Array.t        (* vector of vars          *)

    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> Expression.eval (Expression.subst expr x)) a

    (* give a vector with the value of the objective function
     * of the system at x.
     * 
     * The objective function is defined as f_0^2(x) + f_1^2(x) + ... + f_n^2(x) 
     * where each f_i is equation i in the system 'a' *)
    let objective (a : syst) (x : vect) =
      let vals = Array.map (fun expr -> Expression.eval (Expression. subst expr x)) a in
      Array.fold_left (fun obj x -> obj +. Float.pow x 2.0) 0.0 vals

    (* generate a list of points to evalaute the the objective
     * function at. Points are centered at x and the "star" of points
     * has size alpha. *)
    let get_test_points (x : vect) (alpha : float) = 
      let pairs = Array.mapi (fun i coord ->
                      let x'0 = Array.copy x in
                      let x'1 = Array.copy x in
                      Array.set x'0 i (coord -. alpha);
                      Array.set x'1 i (coord +. alpha);
                      [| x'0; x'1 |]) x in
      Array.concat ([|x|]::(Array.to_list pairs))

    (* returns the index of the smallest element in 'arr' *)
    let smallest (arr : float array) =
      let min = ref Float.infinity in
      let idx = ref 0 in
      Array.iteri (fun i v -> if v < !min
                              then (idx := i;
                                    min := v)) arr;
      !idx
                              
    (* updates the "star" by getting test points, evaluating each
     * one, and moving to the best one or shrinking
     * 
     * Returns : (objective function value, new x, new alpha *)
    let update_star (a : syst) (x : vect) (alpha : float) =
      let test_points = get_test_points x alpha in
      let objective_of_a = objective a in
      let their_values = Array.map objective_of_a test_points in
      (* print_endline ("Objectives: " ^ String.concat ", " (Array.to_list (Array.map Float.to_string their_values))); *)
      let winner = smallest their_values in
      let winning_obj = Array.get their_values winner in
      if winner = 0 (* if the smallest was the original *)
      then (winning_obj, x, alpha *. 0.5)
      else (winning_obj, Array.get test_points winner, alpha)

    (* updates "star" by getting test points in all directions
     * except dim, and moves the star to the best one or rotates it
     * to face the best direction.
     * The direction is |dim| and "forward" if dim is positive and
     * "backward" if dim is negative
     * 
     * Returns : (objective function value, new x, new dim) *)
    let update_biased_star (a : syst) (x : vect) (alpha : float) (dim : int) =
      let test_points = get_test_points x alpha in
      let (fwd_index, rwd_index) =
        if dim < 0
        then (Int.abs dim * 2 + 1, Int.abs dim * 2 + 2)
        else (dim * 2 + 2, dim * 2 + 1) in
      let objective_of_a = objective a in
      let their_values = Array.map objective_of_a test_points in
      Array.set their_values rwd_index Float.infinity; (* do not allow rwd node to win *)
      let winner = smallest their_values in
      let winning_obj = Array.get their_values winner in
      if winner = 0
      then (winning_obj, Array.get test_points fwd_index)
      else (winning_obj, Array.get test_points winner)

    let vary_solution (a : syst) (x0 : vect) (dim : int) (target : float) =
      let rec f = fun x ->
        let coord = Array.get x (Int.abs dim) in
        let signed_dim = if coord < target then dim else -dim in
        let (obj, new_x) = update_biased_star a x 0.1 signed_dim in
        print_endline (Float.to_string obj);
        (* print_endline (String.concat ", " (Array.to_list (Array.map Float.to_string new_x))); *)
        if Float.abs(coord -. target) < 1. || obj > 1. then x
        else f new_x
      in f x0
    
    let solve (a : syst) (x0 : vect) =
      let rec f = fun x alpha ->
        let (obj, new_x, new_alpha) = update_star a x alpha in
        (* print_endline ("alpha: " ^ Float.to_string new_alpha ^ " obj: " ^ Float.to_string obj); *)
        if obj < 0.01 then x
        else f new_x new_alpha
      in f x0 1000.

  end
