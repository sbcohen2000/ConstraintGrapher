type axis = HORIZONTAL
          | VERTICAL

type kind = POINT    of float * float (* x, y *)
          | COLINEAR of axis          (* constrained axis *)

type t = int * kind (* target of the constraint, constraint itself *)

type node = t list

(* returns the variable corresponding to 
 * the node with index 'idx' *)
let node_x (idx : int) = idx * 2
let node_y (idx : int) = idx * 2 + 1

let to_system (this_node : int) (con : t) =
  let target_node, kind = con in
  match kind with
  | POINT (x, y) -> (* this_node's x = x, this_node's y = y *)
     [Expression.Fun ("-", [Expression.X (node_x this_node, Undef); Expression.Const x]);
      Expression.Fun ("-", [Expression.X (node_y this_node, Undef); Expression.Const y])]
  | COLINEAR HORIZONTAL -> (* this_nodes's x = target_node's x *)
     [Expression.Fun ("-", [Expression.X (node_x this_node, Undef);
                            Expression.X (node_x target_node, Undef)])]
  | COLINEAR VERTICAL -> (* this_node's y = target_node's y *)
     [Expression.Fun ("-", [Expression.X (node_y this_node, Undef);
                            Expression.X (node_y target_node, Undef)])]

let to_system (nodes : node list) =
  (* The solution vector contains 2 * |nodes| elements,
   * two for each node representing the node's x and y 
   * coords *)
  let systems = List.mapi (fun idx node ->
                    let f = to_system idx in
                    let systems = List.map f node in
                    List.concat systems) nodes in
  List.concat systems
