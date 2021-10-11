module ISet = Set.Make(Int)

type eqn = {
    f : Expression.t;              (* the original expression *)
    ds : (int * Expression.t) list (* derivative w.r.t x_int  *)
  }

module type Optimizer = sig
  type syst
  type vect

  (* call solve with a custom parameter *)
  val step_solution : syst -> vect -> int * int -> float * float -> vect
  val solve : syst -> vect -> vect
  val objective : syst -> vect -> float
end

module MakeSystem(O : Optimizer) =
  struct
    type syst = eqn Array.t
    type vect = float Array.t

    (* all original functions *)
    let fs (a : syst) = Array.map (fun eqn -> eqn.f) a

    (* give a set with the subscript of each x in the system *)
    let vars (a : syst) =
      Array.fold_left (
          fun set f -> ISet.union (Expression.vars f) set)
        ISet.empty (fs a)

    let n_vars (a : syst) =
      ISet.cardinal (vars a)

    let to_string (a : syst) =
      String.concat "; " (Array.to_list (Array.map Expression.to_string (fs a)))

    let step_solution = O.step_solution
    let solve = O.solve
    let objective = O.objective
  end

module GradientOptimizer =
  struct
    type syst = eqn Array.t
    type vect = float Array.t

    let eval_expr (expr : Expression.t) (x : vect) =
      Expression.eval (Expression.subst expr x)

    (* all original functions *)
    let fs (a : syst) = Array.map (fun eqn -> eqn.f) a
    
    (* give a vector with the value of the system at x *)
    let eval (a : syst) (x : vect) =
      Array.map (fun expr -> eval_expr expr x) (fs a)

    (* give a vector of the derivative of every x_n 
     * in the system *)
    let eval_dfdn (a : syst) (n : int) (x : vect) =
      Array.map (fun f ->
          (* find if the function has a derivative w.r.n x_n *)
          let dx_n = List.find_opt (fun (d, _) -> d = n) f.ds in
          match dx_n with
          | None -> 0.0
          | Some (_, expr) -> eval_expr expr x) a

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

    let line_search (a : syst) (x : vect) (dir : vect) =
      let m     = 0.3  in (* maximum move *)
      let alpha = 0.01 in (* amount to decrease each iteration *)
      let init_objective = objective a x in
      let j = ref 0.0 in
      while
        let x' = move x dir (m -. !j) in
        objective a x' > init_objective
            && Float.abs(!j) < m do
        j := !j +. alpha
      done;
      move x dir (m -. !j);;

    let solve (a : syst) (x0 : vect) =
      let rec f = fun x ->
        let p = grad a x in (* movement direction *)
        let x' = line_search a x p in
        let obj = objective a x' in
        if obj < 0.01 then x'
        else f x' in
      f x0;;

    let pi = 3.14159;;

    exception Empty
    let first = function
      | [] -> raise Empty
      | n::_ -> n;;
 
    let diff (a : vect) (b : vect) =
      Array.map2 (fun x y -> x -. y) a b;;

    let distance (a : vect) (b : vect) =
      let d = diff a b in norm_vect d;;

    let get_probes (x0 : vect) (dims : int * int) =
      let d1, d2 = dims in
      let n_probes = 12 in
      List.init n_probes (fun i ->
          let angle = (Float.of_int i /. Float.of_int n_probes) *. 2. *. pi in
          Array.mapi (fun i v ->
              if i = d1 then v +. 0.3 *. Float.cos angle
              else if i = d2 then v +. 0.3 *. Float.sin angle
              else v) x0);;

    let smallest (l : 'a list) (compare_f : 'a -> float) =
      let rec f = fun lst curr curr_val ->
        match lst with
        | [] -> curr
        | x::xs ->
           let v = compare_f x
           in if v < curr_val then
                f xs x v
              else
                f xs curr curr_val in
      f l (List.nth l 0) Float.infinity;;
      
    let step_solution (a : syst) (x0 : vect) (dims : int * int) (targ : float * float) =
      let d1, d2 = dims in
      let tx, ty = targ in
      (* target is the current solution vector with 
       * d1 and d2 subbed for the target position *)
      let targ = Array.mapi
                   (fun i v -> if i = d1 then tx
                               else if i = d2 then ty
                               else v) x0 in
      let probes = get_probes x0 dims in
      let best_step = smallest probes (distance targ) in
      solve a best_step
  end
