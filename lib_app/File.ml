(* File.ml
 *
 * The File module describes the state of the document
 * which is currently being edited *)

open Core
open Graphics

module Solver = System.MakeSystem(System.DirectOptimizer);;
type node = GraphicsObjects.node * Constraint.t list;;

type t =
  {
    nodes : node Graph.t;
    selected_nodes : int list;
    invalidate : unit -> unit
  };;

let on_click (id : int) =
  print_endline ("Node " ^ Int.to_string id ^ " was clicked.")

let create (invalidate : unit -> unit) =
  let root_node = new GraphicsObjects.node 0 invalidate on_click, [] in
  let (state : t) = { nodes = Graph.create root_node;
                      selected_nodes = [];
                      invalidate } in state

let add_constraint (file : t) (idx : int) (con : Constraint.t) =
  let gobj, constraints = Graph.at file.nodes idx in
  Graph.update_node file.nodes idx (gobj, con::constraints)

let graphics_objects (file : t) = 
  Array.map (fun (gobj, _) -> gobj) (Graph.nodes file.nodes)

let constraint_sets (file : t) =
  Array.map (fun (_, cs) -> cs) (Graph.nodes file.nodes)

let current_system_vector (file : t) =
  let gs = graphics_objects file in
  let positions = List.map (fun obj -> let x, y = obj#get_position in
                                       [| x; y |] ) (Array.to_list gs) in
  Array.concat positions

let update_node_positions (file : t) =
  let gs = graphics_objects file in
  let cs = constraint_sets file in
  let system = Core.Constraint.to_system (Array.to_list cs) in
  let init_guess = current_system_vector file in
  let soln = Solver.solve system init_guess in
  Array.iteri (fun i obj ->
      obj#set_position (Array.get soln (2 * i), Array.get soln (2 * i + 1)))
    gs
  
let draw (file : t) (cr : Cairo.context) =
  update_node_positions file;
  let gs = graphics_objects file in
  Array.iter (fun obj -> obj#render cr) gs

let handle (file : t) (ev : EventParser.event) =
  let gs = graphics_objects file in
  match ev with EventParser.DRAG _details -> ()
              | _ -> Array.iter (fun obj -> obj#handle ev) gs

