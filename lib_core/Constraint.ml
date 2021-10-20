
type axis = Horizontal
          | Vertical

type t = Point  of float * float (* x, y *)
       | Axis   of int * axis    (* target, constrained axis *)
       | Radial of int * float   (* target, distance *)
       | Offset of int * float * float (* target, x, y *)
       | Colinear of int * int (* target a, target b *)

(* returns the variable corresponding to 
 * the node with index 'idx' *)
let node_x (idx : int) = idx * 2
let node_y (idx : int) = idx * 2 + 1

let targets (con : t) =
  match con with
  | Point _ -> []
  | Axis (targ, _) -> [targ]
  | Radial (targ, _) -> [targ]
  | Offset (targ, _, _) -> [targ]
  | Colinear (targ_a, targ_b) -> [targ_a; targ_b]

let target_string_opt (con : t) =
  match targets con with
  | [] -> None
  | ns -> Some (String.concat ", " (List.map Int.to_string ns))

let description (con : t) =
  match con with
  | Point (x, y) -> Printf.sprintf "Locked to (%0.1f, %0.1f)" x y
  | Axis (_, Horizontal) -> "Horizontal lock"
  | Axis (_, Vertical) -> "Vertical lock"
  | Radial (_, r) -> Printf.sprintf "Distance of %0.1f" r
  | Offset (_, x, y) -> Printf.sprintf "Offset (%0.1f, %0.1f)" x y
  | Colinear _ -> "Colinear"

let to_string (con : t) =
  match con with
  | Point (x, y) ->
     "Point (" ^ Float.to_string x ^ ", " ^ Float.to_string y ^ ")"
  | Axis (targ, Horizontal) ->
     "Axis @ " ^ Int.to_string targ ^ " (Horizontal)"
  | Axis (targ, Vertical) ->
     "Axis @ " ^ Int.to_string targ ^ " (Vertical)"
  | Radial (targ, r) ->
     "Radial @ " ^ Int.to_string targ ^ " r: " ^ Float.to_string r
  | Offset (targ, x, y) ->
     "Offset @ " ^ Int.to_string targ ^ "("
     ^ Float.to_string x ^ ", " ^ Float.to_string y ^ ")"
  | Colinear (targ_a, targ_b) ->
     "Colinear @ " ^ Int.to_string targ_a ^ ", "
     ^ Int.to_string targ_b

(* returns a tuple of five expressions, the first being an expression
 * for the distance between the points (x1, y1) and (x2, y2), and the
 * remaining four being derivatives of this expression w.r.t x1, y1,
 * x2, and y2 in that order. *)        
let distance_expr (x1, y1 : Expression.t * Expression.t) (x2, y2 : Expression.t * Expression.t) =
  let open Expression in
  let f = Mon (SQRT, Bin (ADD, Mon (SQR, Bin (SUB, x2, x1)),
                          Mon (SQR, Bin (SUB, y2, y1)))) in
  let denom = Mon (SQRT, Bin (ADD, Mon (SQR, Bin (SUB, x2, x1)),
                              Mon (SQR, Bin (SUB, y2, y1)))) in
  let d_x2 = Bin (DIV, Bin (SUB, x2, x1), denom) in
  let d_x1 = Mon (NEG, Bin (DIV, Bin (SUB, x2, x1), denom)) in
  let d_y2 = Bin (DIV, Bin (SUB, y2, y1), denom) in
  let d_y1 = Mon (NEG, Bin (DIV, Bin (SUB, y2, y1), denom)) in
  (f, d_x1, d_y1, d_x2, d_y2)

let con_to_system (this_node : int) (con : t) =
  let open Expression in
  match con with
  | Point (x, y) -> (* this_node's x = x, this_node's y = y *)
     let (eqn1 : System.eqn) =
       {
         f = Bin (SUB, X (node_x this_node), Const x);
         ds = [
             (node_x this_node, Const 1.0)
           ]
       } in
     let (eqn2 : System.eqn) =
       {
         f = Bin (SUB, X (node_y this_node), Const y);
         ds = [
             (node_y this_node, Const 1.0)
           ]
       } in
     [ eqn1; eqn2 ]
  | Axis (target_node, Horizontal) -> (* this_nodes's y = target_node's y *)
     let (eqn : System.eqn) =
       {
         f = Bin (SUB, X (node_y this_node), X (node_y target_node));
         ds = [
             (node_y this_node,   Const   1.0);
             (node_y target_node, Const (-1.0));
           ]
       }
     in [ eqn ]
  | Axis (target_node, Vertical) -> (* this_node's x = target_node's x *)
     let (eqn : System.eqn) =
       {
         f = Bin (SUB, X (node_x this_node), X (node_x target_node));
         ds = [
             (node_x this_node,   Const   1.0);
             (node_x target_node, Const (-1.0));
           ]
       }
     in [ eqn ]
  | Radial (target_node, r) -> (* this_node is distance r from target node *)
     let target_x = X (node_x target_node) in
     let target_y = X (node_y target_node) in
     let this_x = X (node_x this_node) in
     let this_y = X (node_y this_node) in
     let f, d_x1, d_y1, d_x2, d_y2 = distance_expr (this_x, this_y) (target_x, target_y) in
     let (eqn : System.eqn) =
       {
         f = Bin (SUB, f, Const r);
         ds = [
             (node_x target_node, d_x2);
             (node_x this_node,   d_x1);
             (node_y target_node, d_y2);
             (node_y this_node,   d_y1)
           ]
       }
     in [ eqn ]
  | Offset (target_node, x, y) ->
     let (eqn1 : System.eqn) =
       {
         f = Bin (SUB, X (node_x this_node),
                  Bin (ADD, X(node_x target_node), Const x));
         ds = [
             (node_x this_node, Const 1.);
             (node_x target_node, Const (-1.));
           ]
       } in
     let (eqn2 : System.eqn) =
       {
         f = Bin (SUB, X (node_y this_node),
                  Bin (ADD, X(node_y target_node), Const y));
         ds = [
             (node_y this_node, Const 1.);
             (node_y target_node, Const (-1.));
           ]
       }
     in [ eqn1; eqn2 ]
  | Colinear (targ_a, targ_b) ->
     (* let d(x, y) be the distance between x and y.
      * Then, a, b, and c are colinear if d(a, c) = d(a, b) + d(b, c) *)
     let ax = node_x this_node
     and ay = node_y this_node
     and bx = node_x targ_a
     and by = node_y targ_a
     and cx = node_x targ_b
     and cy = node_y targ_b in
     let d_ac, d_ac_dax, d_ac_day, d_ac_dcx, d_ac_dcy =
       distance_expr (X ax, X ay) (X cx, X cy)
     and d_ab, d_ab_dax, d_ab_day, d_ab_dbx, d_ab_dby =
       distance_expr (X ax, X ay) (X bx, X by)
     and d_bc, d_bc_dbx, d_bc_dby, d_bc_dcx, d_bc_dcy =
       distance_expr (X bx, X by) (X cx, X cy) in
     let (eqn : System.eqn) = {
         f = Bin (SUB, Bin (ADD, d_ab, d_bc), d_ac);
         ds = [
             (node_x this_node, Bin (ADD, d_ab_dax, Mon (NEG, d_ac_dax)));
             (node_y this_node, Bin (ADD, d_ab_day, Mon (NEG, d_ac_day)));
             (node_x targ_a, Bin (ADD, d_ab_dbx, d_bc_dbx));
             (node_y targ_a, Bin (ADD, d_ab_dby, d_bc_dby));
             (node_x targ_b, Bin (ADD, d_bc_dcx, Mon (NEG, d_ac_dcx)));
             (node_y targ_b, Bin (ADD, d_bc_dcy, Mon (NEG, d_ac_dcy)))
           ]
       }
     in [ eqn ];;

let to_system (constraints : t list list) =
  (* The solution vector contains 2 * |nodes| elements,
   * two for each node representing the node's x and y 
   * coords *)
  let systems = List.mapi (fun idx node ->
                    let f = con_to_system idx in
                    let systems = List.map f node in
                    List.concat systems) constraints in
  Array.of_list (List.concat systems)
