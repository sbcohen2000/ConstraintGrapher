Random.self_init ()
module ISet = Set.Make(Int)

module type Optimizer = sig
  type syst = Expression.t Array.t Array.t
  type vect = float Array.t

  (* call solve with a custom parameter *)
  val vary_solution : syst -> vect -> int -> float -> vect
  val solve : syst -> vect -> vect
  val objective : syst -> vect -> float
end

module MakeSystem(O : Optimizer) =
  struct
    type syst = Expression.t Array.t Array.t (* the system of equations *)
    type vect = float Array.t                (* vector of vars          *)

    (* give a set with the subscript of each x in the system *)
    let vars (a : syst) =
      Array.fold_left (
          fun set exprs -> ISet.union (Expression.vars (Array.get exprs 0)) set)
        ISet.empty a

    let n_vars (a : syst) =
      ISet.cardinal (vars a)

    let to_string (a : syst) =
      String.concat "; " (Array.to_list (Array.map Expression.to_string (Array.get a 0)))

    let vary_solution = O.vary_solution
    let solve = O.solve
    let objective = O.objective
  end

module GradientOptimizer =
  struct
    type eqn = Expression.t Array.t
    type syst = eqn Array.t    (* the system of equations *)
    type vect = float Array.t  (* vector of vars          *)

    (* the original function *)
    let fx (a : Expression.t Array.t) = Array.get a 0

    (* all original functions *)
    let fs (a : syst) = Array.map fx a

    let eval_expr (expr : Expression.t) (x : vect) =
      Expression.eval (Expression.subst expr x)

    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> eval_expr expr x) (fs a)

    (* give a vector of the derivative of every x_n 
     * in the system *)
    let eval_dfdn (a : syst) (n : int) (x : vect) =
      Array.map (fun f ->
          eval_expr (Array.get f (n + 1)) x) a

    let sum_vect (x : vect) =
      Array.fold_left (fun sum x -> sum +. x) 0.0 x

    let norm_vect (x : vect) =
      Array.fold_left (fun sum x -> sum +. (x *. x)) 0.0 x
    
    let objective (a : syst) (x : vect) =
      let vals = Array.map (fun expr -> Expression.eval (Expression.subst expr x)) (fs a) in
      norm_vect vals
    
    let grad (a : syst) (x : vect) =
      let g = eval a x in
      Array.mapi (fun i _ ->
          let j_i = eval_dfdn a i x in
          let gj_i = Array.map2 (fun a b -> a *. b) g j_i in
          sum_vect gj_i
        ) x
      
    let move (x0 : vect) (p : vect) (amount : float) =
      let ap = Array.map (fun x -> x *. amount) p in
      Array.map2 (fun x y -> x -. y) x0 ap
    
    let line_search (a : syst) (x : vect) =
      let m     = 0.1  in (* maximum move *)
      let alpha = 0.01 in (* amount to decrease each iteration *)
      let p = grad a x  in (* movement direction *)
      print_endline (String.concat ", " (Array.to_list (Array.map Float.to_string x)) ^ ", " ^ String.concat ", " (Array.to_list (Array.map Float.to_string p)));
      let init_objective = objective a x in
      let x' = ref x  in
      let j = ref 0.0 in
      while init_objective <= objective a !x' && Float.abs(!j) < m do
        let amount = m -. !j in
        (* print_endline ("F: " ^ Float.to_string (objective a !x') ^ " alpha: " ^ Float.to_string (amount)); *)
        x' := move !x' p amount;
        j := !j +. alpha
      done;
      !x'
    
    let solve (a : syst) (x0 : vect) =
      let rec f = fun x ->
        let x' = line_search a x in
        let obj = objective a x' in
        if obj < 0.01 then x'
        else f x' in
      f x0

    let vary_solution (_a : syst) (x0 : vect) (_dim : int) (_target : float) = x0
  end

module DirectOptimizer =
  struct
    type syst = Expression.t Array.t Array.t (* the system of equations *)
    type vect = float Array.t (* vector of vars          *)

    let fx (a : Expression.t Array.t) = Array.get a 0

    (* all original functions *)
    let fs (a : syst) = Array.map fx a
    
    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> Expression.eval (Expression.subst expr x)) (fs a)

    (* give a vector with the value of the objective function
     * of the system at x.
     * 
     * The objective function is defined as f_0^2(x) + f_1^2(x) + ... + f_n^2(x) 
     * where each f_i is equation i in the system 'a' *)
    let objective (a : syst) (x : vect) =
      let vals = Array.map (fun expr -> Expression.eval (Expression. subst expr x)) (fs a) in
      Array.fold_left (fun obj x -> obj +. Float.pow x 2.0) 0.0 vals

    type point_gen = vect -> float -> float array
    let test_point_generator (n_dims : int) =
      let vect = Array.create_float ((2 * n_dims + 1) * n_dims) in
      fun (x : vect) (alpha : float) ->
      Array.blit x 0 vect 0 n_dims;
      (* each point in the star *)
      for i = 0 to n_dims - 1 do
        let r = Random.float 0.1 -. 0.05 in
        begin
          (* place x vector into the ith point of the star *)
          Array.blit x 0 vect ((2 * i + 1) * n_dims) n_dims;
          Array.blit x 0 vect ((2 * i + 2) * n_dims) n_dims;
          (* translate the ith component by alpha *)
          Array.set vect ((2 * i + 1) * n_dims + i) (Array.get x i -. alpha +. r);
          Array.set vect ((2 * i + 2) * n_dims + i) (Array.get x i +. alpha +. r);
        end
      done;
      vect

    (* extracts a single point from the array of 
     * test points generated by test_point_generator *)
    let point_n (points : float array) (n_dims : int) (n : int) =
      Array.sub points (n_dims * n) n_dims
    
    let map_points (points : float array) (n_dims : int) (f : vect -> float) =
      let n_points = n_dims * 2 + 1 in
      Array.init n_points (fun n -> f  (point_n points n_dims n))

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
    let update_star (get_test_points : point_gen)
          (a : syst) (x : vect) (alpha : float) =
      let test_points = get_test_points x alpha in
      let objective_of_a = objective a in
      let n_dims = Array.length x in
      let their_values = map_points test_points n_dims objective_of_a in
      let winner = smallest their_values in
      let winning_obj = Array.get their_values winner in
      if winner = 0 (* if the smallest was the original *)
      then (winning_obj, x, alpha *. 0.5)
      else (winning_obj, point_n test_points n_dims winner, alpha)

    type direction = Fwd of int | Rwd of int
    (* updates "star" by getting test points in all directions
     * except dim, and moves the star to the best one
     * 
     * Returns : (objective function value, new x) *)
    let update_biased_star (get_test_points : point_gen)
          (a : syst) (x : vect) (alpha : float) (dim : direction) =
      let test_points = get_test_points x alpha in
      let (fwd_index, rwd_index) =
        match dim with
        | Fwd i -> (i * 2 + 2, i * 2 + 1)
        | Rwd i -> (i * 2 + 1, i * 2 + 2) in
      let objective_of_a = objective a in
      let n_dims = Array.length x in
      let their_values = map_points test_points n_dims objective_of_a in
      Array.set their_values rwd_index Float.infinity; (* do not allow rwd node to win *)
      let winner = smallest their_values in
      let winning_obj = Array.get their_values winner in
      if winner = 0
      then (winning_obj, point_n test_points n_dims fwd_index)
      else (winning_obj, point_n test_points n_dims winner)

    let vary_solution (a : syst) (x0 : vect) (dim : int) (target : float) =
      let gtp = test_point_generator (Array.length x0) in
      let iters = ref 0 in
      let x     = ref x0 in
      let coord = ref (Array.get x0 (Int.abs dim)) in
      let obj   = ref 0. in
      while Float.abs(target -. !coord) > 1.
            && !obj < 1.
            && !iters < 1000 do
        iters := !iters + 1;
        let directed_dim = if !coord < target then Fwd dim else Rwd dim in
        coord := Array.get !x dim;
        let (new_obj, new_x) = update_biased_star gtp a !x 0.1 directed_dim in
        obj := new_obj;
        x := new_x;
        (* Printf.printf "obj: %.3f\n, target: %.1f, curr: %.1f" !obj target !coord; *)
      done;
      !x

    let solve (a : syst) (x0 : vect) =
      let gtp = test_point_generator (Array.length x0) in
      let x = ref x0 in
      let obj = ref Float.infinity in
      let alpha = ref 1000. in
      while !obj > 0.01 do
        let (new_obj, new_x, new_alpha) = update_star gtp a !x !alpha in
        obj := new_obj;
        x := new_x;
        alpha := new_alpha;
      done;
      !x
    
  end
