(* File.ml
 *
 * The File module describes the state of the document
 * which is currently being edited *)

open Core
open Graphics

module Int_set = Set.Make(Int);;
module Solver = System.MakeSystem(System.DirectOptimizer);;
type node = GraphicsObjects.node * Constraint.t list;;

type state =
  {
    nodes          : node array;
    connections    : bool Graph.t;
    selected_nodes : Int_set.t;
    invalidator    : unit -> unit
  };;

type t = state ref;;

let create (invalidator : unit -> unit) =
  let (state : t) = ref { nodes = [| |];
                          connections = Graph.empty;
                          selected_nodes = Int_set.empty;
                          invalidator } in state

let is_selected (state : t) (id : int) =
  Option.is_some (Int_set.find_opt id !state.selected_nodes)

let on_click (state : t) (id : int) =
  let selected' = if is_selected state id
                  then Int_set.remove id !state.selected_nodes
                  else Int_set.add id !state.selected_nodes in
  state := { !state with selected_nodes = selected' };
  !state.invalidator ()
    
exception Invariant_Violation
let n_nodes (file : t) =
  if Array.length !file.nodes = Graph.size !file.connections
  then Array.length !file.nodes
  else raise Invariant_Violation

let add_node (file : t) =
  let new_id = n_nodes file in
  let g_obj = new GraphicsObjects.node new_id !file.invalidator (on_click file) in
  file := { !file with nodes = Array.append !file.nodes [| (g_obj, []) |];
                       connections = Graph.expand !file.connections; };
  new_id
                       
let add_constraint (file : t) (idx : int) (con : Constraint.t) =
  let state = !file in
  let gobj, constraints = Array.get state.nodes idx in
  Array.set state.nodes idx (gobj, con::constraints)

let graphics_objects (file : t) = 
  Array.map (fun (gobj, _) -> gobj) !file.nodes

let constraint_sets (file : t) =
  Array.map (fun (_, cs) -> cs) !file.nodes

let current_system_vector (file : t) =
  let gs = graphics_objects file in
  let positions = List.map (fun obj -> let x, y = obj#get_position in
                                       [| x; y |] ) (Array.to_list gs) in
  Array.concat positions

let prepare_system (file : t) =
  let cs = constraint_sets file in
  let system = Core.Constraint.to_system (Array.to_list cs) in
  let init_guess = current_system_vector file in
  system, init_guess

let solve_node_positions (file : t) =
  let gs = graphics_objects file in
  let system, init_guess = prepare_system file in
  let soln = Solver.solve system init_guess in
  Array.iteri (fun i obj ->
      obj#set_position (Array.get soln (2 * i), Array.get soln (2 * i + 1)))
    gs

let update_node_positions (file : t) (dim : int) (target : float) =
  let gs = graphics_objects file in  
  let system, init_guess = prepare_system file in
  let soln = Solver.vary_solution system init_guess dim target in
  Array.iteri (fun i obj ->
      obj#set_position (Array.get soln (2 * i), Array.get soln (2 * i + 1)))
    gs

let draw (file : t) (cr : Cairo.context) =
  solve_node_positions file;
  let gs = graphics_objects file in
  Array.iteri (fun idx obj ->
      if is_selected file idx
      then obj#select () else obj#deselect ();
      obj#render cr) gs

type drag_primary = Horizontal | Vertical
let handle_drag (file : t) (start : float * float) (finish : float * float) =
  let state = !file in
  match Int_set.elements state.selected_nodes with
  | [node_id] -> 
     (if Int_set.cardinal state.selected_nodes = 1
     then let sx, sy = start in
          let fx, fy = finish in
          let direction = if Float.abs (fx -. sx) > Float.abs (fy -. sy)
                          then Horizontal else Vertical in
          let dim, target = match direction with
            | Horizontal -> node_id * 2, fx
            | Vertical -> node_id * 2 + 1, fy in
          update_node_positions file dim target;
          state.invalidator ())
  | _ -> () (* can't drag if more than one node is selected *)

let handle (file : t) (ev : EventParser.event) =
  let gs = graphics_objects file in
  match ev with
  | EventParser.Drag details ->
     handle_drag file details.start details.finish
  | _ ->
     let handled = Array.map (fun obj -> obj#handle ev) gs in
     let one_handled = Array.exists (fun x -> x) handled in
     if one_handled then ()
     else
       (file := { !file with selected_nodes = Int_set.empty };
        !file.invalidator ())
     

