
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

let con_to_system (this_node : int) (con : t) =
  let open Expression in
  match con with
  | Point (x, y) -> (* this_node's x = x, this_node's y = y *)
     [Fun ("-", [X (node_x this_node, Undef); Const x]);
      Fun ("-", [X (node_y this_node, Undef); Const y])]
  | Axis (target_node, Horizontal) -> (* this_nodes's y = target_node's y *)
     [Fun ("-", [X (node_y this_node, Undef);
                            X (node_y target_node, Undef)])]
  | Axis (target_node, Vertical) -> (* this_node's x = target_node's x *)
     [Fun ("-", [X (node_x this_node, Undef);
                            X (node_x target_node, Undef)])]
  | Radial (target_node, r) -> (* this_node is distance r from target node *)
     let target_x = X (node_x target_node, Undef) in
     let target_y = X (node_y target_node, Undef) in
     let this_x = X (node_x this_node, Undef) in
     let this_y = X (node_y this_node, Undef) in
     [Fun ("-", [Fun ("sqrt", [Fun ("+", [Fun ("sqr", [Fun ("-", [target_x; this_x])]);
                                          Fun ("sqr", [Fun ("-", [target_y; this_y])])])]);
                 Const r])]
  | Offset (target_node, x, y) ->
     [Fun ("-", [X (node_x this_node, Undef);
                 Fun ("+", [X(node_x target_node, Undef); Const x])]);
      Fun ("-", [X (node_y this_node, Undef);
                 Fun ("+", [X(node_y target_node, Undef); Const y])])]
  | Colinear (targ_a, targ_b) ->
     let x = X (node_x this_node, Undef) in
     let y = X (node_y this_node, Undef) in
     let x_1 = X (node_x targ_a, Undef) in
     let y_1 = X (node_y targ_a, Undef) in
     let x_2 = X (node_x targ_b, Undef) in
     let y_2 = X (node_y targ_b, Undef) in
     let m = Fun ("/", [Fun ("-", [y_1; y_2]); Fun ("-", [x_1; x_2])]) in
     [Fun ("+", [Fun ("-", [Fun ("*", [m; Fun ("-", [x; x_1])]); y]); y_1])]

let to_system (constraints : t list list) =
  (* The solution vector contains 2 * |nodes| elements,
   * two for each node representing the node's x and y 
   * coords *)
  let systems = List.mapi (fun idx node ->
                    let f = con_to_system idx in
                    let systems = List.map f node in
                    List.concat systems) constraints in
  Array.of_list (List.concat systems)
