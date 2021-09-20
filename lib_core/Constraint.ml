
type axis = Horizontal
          | Vertical

type t = Point    of float * float (* x, y *)
       | Colinear of int * axis    (* target, constrained axis *)
       | Radial   of int * float   (* target, distance *)

(* returns the variable corresponding to 
 * the node with index 'idx' *)
let node_x (idx : int) = idx * 2
let node_y (idx : int) = idx * 2 + 1

let target_string_opt (con : t) =
  match con with
  | Point _ -> None
  | Colinear (targ, _) -> Some (Int.to_string targ)
  | Radial (targ, _) -> Some (Int.to_string targ)

let description (con : t) =
  match con with
  | Point (x, y) -> Printf.sprintf "Locked to (%0.2f, %0.2f)" x y
  | Colinear (_, Horizontal) -> "Horizontally constrained"
  | Colinear (_, Vertical) -> "Vertically constrained"
  | Radial (_, r) -> Printf.sprintf "Constrained to distance of %0.2f" r

let to_string (con : t) =
  match con with
  | Point (x, y) ->
     "Point (" ^ Float.to_string x ^ ", " ^ Float.to_string y ^ ")"
  | Colinear (targ, Horizontal) ->
     "Colinear @ " ^ Int.to_string targ ^ " (Horizontal)"
  | Colinear (targ, Vertical) ->
     "Colinear @ " ^ Int.to_string targ ^ " (Vertical)"
  | Radial (targ, r) ->
     "Radial @ " ^ Int.to_string targ ^ " r: " ^ Float.to_string r

let con_to_system (this_node : int) (con : t) =
  let open Expression in
  match con with
  | Point (x, y) -> (* this_node's x = x, this_node's y = y *)
     [Fun ("-", [X (node_x this_node, Undef); Const x]);
      Fun ("-", [X (node_y this_node, Undef); Const y])]
  | Colinear (target_node, Horizontal) -> (* this_nodes's y = target_node's y *)
     [Fun ("-", [X (node_y this_node, Undef);
                            X (node_y target_node, Undef)])]
  | Colinear (target_node, Vertical) -> (* this_node's x = target_node's x *)
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

let to_system (constraints : t list list) =
  (* The solution vector contains 2 * |nodes| elements,
   * two for each node representing the node's x and y 
   * coords *)
  let systems = List.mapi (fun idx node ->
                    let f = con_to_system idx in
                    let systems = List.map f node in
                    List.concat systems) constraints in
  Array.of_list (List.concat systems)
