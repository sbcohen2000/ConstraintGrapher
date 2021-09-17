
type axis = Horizontal
          | Vertical

type t = Point    of float * float (* x, y *)
       | Colinear of int * axis    (* target, constrained axis *)
       | Radial   of int * float   (* target, distance *)

(* returns the variable corresponding to 
 * the node with index 'idx' *)
let node_x (idx : int) = idx * 2
let node_y (idx : int) = idx * 2 + 1

let to_system (this_node : int) (con : t) =
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
                    let f = to_system idx in
                    let systems = List.map f node in
                    List.concat systems) constraints in
  Array.of_list (List.concat systems)
