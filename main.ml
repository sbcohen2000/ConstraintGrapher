type node = { constraints : Core.Constraint.t list;
              g_obj : Graphics.GraphicsObjects.node;
              id : int };;
let (nodes : node list ref) = ref [];;
let (selection : int list ref) = ref [];;

module Solver = Core.System.MakeSystem(Core.System.DirectOptimizer);;

let node_n (id : int) =
  List.find (fun node -> id = node.id) !nodes

exception BadId of int
let relative_id (id : int) =
  let sorted_nodes = List.sort (fun a b -> Int.compare a.id b.id) !nodes in
  let rec find = fun i ns ->
    match ns with
    | [] -> raise (BadId id)
    | n::ns -> if n.id = id then i
               else find (i + 1) ns in
  find 0 sorted_nodes;;

(* ==== ACTIONS ============================================================= *)

(* When the UI requires that the user provides 
 * some text input to complete a command (e.g. a 
 * parameter input or user specified function), a pending
 * action is created. When the user completes the action,
 * the pending action is completed by evaluating the action
 * with the string typed by the user. If the user cancels, 
 * the action is removed and the action is never evaluated. *)
type action = { f : string -> bool;
                instruction : string };;
let (pending_action : action option ref) = ref None;;

type input_group = {
    label : GMisc.label;
    entry : GEdit.entry };;

let register_action (act : action) (iput : input_group) =
  pending_action := Some act;
  iput.label#set_text act.instruction;
  iput.entry#set_text "";
  iput.entry#set_visible true;
  iput.entry#set_has_focus true;;

let on_entry_activated (iput : input_group) () =
  let act_opt = !pending_action in
  pending_action := None;
  match act_opt with
  | None -> ()
  | Some act -> 
     let str = iput.entry#text in
     let succ = act.f str in
     let label_str =
       if succ then act.instruction ^ " (" ^ str ^ ")"
       else act.instruction ^ " invalid input \"" ^ str ^ "\"" in
     iput.label#set_text label_str;
     iput.entry#set_visible false;;
     

(* ==== CONSTRAINT TABLE ==================================================== *)

let c_table_cols = new GTree.column_list;;
let target = c_table_cols#add Gobject.Data.string_option;;
let description = c_table_cols#add Gobject.Data.string;;

let generate_model () =
  let store = GTree.tree_store c_table_cols in
  let rec last = fun lst ->
    match lst with
    | [] -> None
    | [n] -> Some n
    | _::ns -> last ns in
  let selected_node = last !selection in
  let cs = match selected_node with
    | None -> [] 
    | Some n -> let node = node_n n in node.constraints in
  List.iter (fun con ->
      let row = store#append () in
      store#set ~row ~column:target
        (Core.Constraint.target_string_opt con);
      store#set ~row ~column:description
        (Core.Constraint.description con)) cs;
  store;;

let set_model (table_view : GTree.view) =
  let model = generate_model () in
  let model = model#coerce in
  table_view#set_model (Some model);;

(* ==== GEOMETRIC SOLVING =================================================== *)

let draw cr =
  (* white background *)
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  List.iter (fun node ->
      node.g_obj#render cr) !nodes;
  true;;

let ordered_constraints () =
  let sorted_nodes = List.sort
                       (fun a b -> Int.compare a.id b.id)
                       !nodes in
  List.fold_right (fun node cs ->
      node.constraints::cs) sorted_nodes [];;

let system_vector () =
  let size = 2 * (List.length !nodes) in
  let vect = Array.create_float size in
  List.iter (fun node ->
      let x, y = node.g_obj#get_position in
      let rel_id = relative_id node.id in
      Array.set vect (rel_id * 2) x;
      Array.set vect (rel_id * 2 + 1) y) !nodes;
  vect

let solve () =
  let cs = ordered_constraints () in
  List.iteri (fun idx cs -> print_endline (Int.to_string idx ^ ":");
                            List.iter (fun con ->
                                print_endline ("\t" ^ (Core.Constraint.to_string con)))
                              cs)
    cs;
  let system = Core.Constraint.to_system cs in
  Array.iter (fun expr -> print_endline (Core.Expression.to_string expr)) system;
  let init_guess = system_vector () in
  let soln = Solver.solve system init_guess in
  List.iter (fun node ->
      let rel_id = relative_id node.id in
      node.g_obj#set_position (Array.get soln (2 * rel_id),
                               Array.get soln (2 * rel_id + 1)))
    !nodes;;

let create_solution_updater (dim : int) =
  let cs = ordered_constraints () in
  let system = Core.Constraint.to_system cs in
  fun (target : float) -> 
  let init_guess = system_vector () in
  let soln = Solver.vary_solution system init_guess dim target in
  List.iter (fun node ->
      let rel_id = relative_id node.id in
      node.g_obj#set_position (Array.get soln (2 * rel_id),
                               Array.get soln (2 * rel_id + 1)))
    !nodes;;

let update_selection (id : int) (table_view : GTree.view) =
  let node = node_n id in
  if List.exists (fun i -> i = id) !selection
  then (print_endline ("deselecting " ^ Int.to_string id);
        selection := List.filter (fun i -> not (i = id)) !selection;
        node.g_obj#set_selection Graphics.GraphicsObjects.None)
  else (print_endline ("selecting " ^ Int.to_string id);
        match !selection with
        | [] -> 
           selection := id::!selection;
           node.g_obj#set_selection Graphics.GraphicsObjects.Primary;
        | _ ->
           selection := id::!selection;
           node.g_obj#set_selection Graphics.GraphicsObjects.Secondary);
  set_model table_view;;

let deselect_all (table_view : GTree.view) =
  print_endline "deselecting all";
  selection := [];
  List.iter (fun node -> node.g_obj#set_selection Graphics.GraphicsObjects.None) !nodes;
  set_model table_view;;

(* ==== MOUSE CONTROLS ====================================================== *)

(* returns the node id and the ordered id
 * of the clicked node *)
let node_underneath (x, y : float * float) =
  let sorted_nodes = List.sort (fun a b -> Int.compare a.id b.id) !nodes in
  let rec find = fun i ns ->
    match ns with
    | [] -> None
    | n::ns -> if n.g_obj#is_inside (x, y) then Some (n.id, i)
               else find (i + 1) ns in
  find 0 sorted_nodes;;

type updaters = { x_updater : float -> unit;
                  y_updater : float -> unit }
type drag = Pending of updaters (* clicked down, not dragged yet *)
          | Valid of float * float * updaters (* dragging *)
          | Invalid;; (* clicked on nothing *)
let current_drag = ref Invalid;;

let on_mouse_move invalidate event =
  let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  (match !current_drag with
   | Pending upd -> current_drag := Valid (x, y, upd)
   | Valid (_, _, upd) ->
      upd.x_updater x;
      upd.y_updater y;
      invalidate ();
   | Invalid -> ());
  true;;

let on_mouse_down _invalidate event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  let clicked = node_underneath (x, y) in
  (match clicked with
   | Some (_, ordered_id) ->
      let x_dim = ordered_id * 2 in
      let y_dim = ordered_id * 2 + 1 in
      print_endline ("starting to drag with x_dim: " ^ Int.to_string x_dim ^ ", y_dim: " ^ Int.to_string y_dim);
      let (upd : updaters) = {
          x_updater = create_solution_updater x_dim;
          y_updater = create_solution_updater y_dim } in
      current_drag := Pending upd
   | None -> current_drag := Invalid);
  true;;

let on_mouse_up invalidate table_view event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  (* check if we clicked any nodes *)
  let clicked = node_underneath (x, y) in
  (match clicked, !current_drag with
   | (Some (id, _), Valid (sx, sy, _)) ->
      let d = Graphics.Geometry.Point.distance (x, y) (sx, sy) in
      (* if the user dragged for fewer than 2 units, treat it as a click *)
      if d < 2. then
        update_selection id table_view
      else ()
   | (Some (id, _), _) ->
      update_selection id table_view
   | (None, Valid _) -> ()
   | (None, _) -> deselect_all table_view);
  current_drag := Invalid;
  invalidate ();
  true;;

(* ==== TOOLBAR BUTTONS ===================================================== *)

let new_id () =
  let max = ref 0 in
  List.iter (fun node ->
      if node.id > !max then max := node.id) !nodes;
  !max + 1;;

let on_add_pressed invalidate _entry () =
  let g_obj = new Graphics.GraphicsObjects.node () in
  g_obj#set_position (Random.float 250., Random.float 200.);
  let new_node = {
      constraints = [];
      g_obj; id = new_id (); } in
  nodes := new_node::!nodes;
  invalidate ();
  print_endline "added node";;

(* apply_con is a function that accepts the target node and returns
 * a constraint applying that node as the target *)
let add_constraints_to_selected (apply_con : int -> Core.Constraint.t) =
  match !selection with
  | [] -> ()
  | [_] -> ()
  | n::ns ->
     let con = apply_con (relative_id n) in
     nodes := List.map (fun node ->
                  (* if node in ns, add con to its list of constraints *)
                  let cs' =
                    if List.exists (fun sel -> sel = node.id) ns
                    then con::node.constraints
                    else node.constraints in
                  { node with constraints = cs' }) !nodes

let on_delete_pressed invalidate _iput () =
  let delete_node = fun id ->
    let rel_id = relative_id id in
    (* delete the node from the list of nodes *)
    nodes := List.filter (fun node -> not (node.id = id)) !nodes;
    (* delete every constraint that references the deleted node *)
    nodes := List.map (fun node ->
                 let cs' = List.filter (fun con ->
                               match Core.Constraint.target_opt con with
                               | Some targ -> not (targ = rel_id)
                               | None -> true) node.constraints in
                 { node with constraints = cs' }) !nodes; in
  List.iter delete_node !selection;
  selection := [];
  invalidate ();;

let on_hor_con_pressed invalidate _iput () =
  add_constraints_to_selected (fun n -> Core.Constraint.Colinear (n, Core.Constraint.Horizontal));
  solve ();
  invalidate ();
  print_endline "horizontal constraint pressed";;

let on_vert_con_pressed invalidate _iput () =
  add_constraints_to_selected (fun n -> Core.Constraint.Colinear (n, Core.Constraint.Vertical));
  solve ();
  invalidate ();
  print_endline "vertical constraint pressed";;

let on_lock_con_pressed _invalidate _iput () =
  nodes := List.map (fun node ->
               let cs' = if List.exists (fun sel -> sel = node.id) !selection
                         then let x, y = node.g_obj#get_position in
                              let con = Core.Constraint.Point (x, y) in
                              con::node.constraints
                         else node.constraints in
             { node with constraints = cs' }) !nodes

let on_rad_con_pressed invalidate (iput : input_group) () =
  let action = (fun str ->
      let rad = Float.of_string_opt str in
      match rad with
      | Some r ->
         begin
           add_constraints_to_selected (fun n -> Core.Constraint.Radial (n, r));
           solve ();
           invalidate ();
           true
         end
      | None -> false) in
  register_action { f = action;
                    instruction = "radius:" } iput;
  print_endline "radial constraint pressed";;

let make_toolbar_button (image_name : string) (label : string) =
  let path = "/home/sam/Documents/ConstraintGrapher/resources/" ^ image_name in
  let image = GMisc.image ~file:path () in
  let button = GButton.tool_button ~label ~use_underline:true () in
  button#set_icon_widget image#coerce;
  button;;

type toolbar_cb = (unit -> unit) -> input_group -> unit -> unit;;
type toolbar_item = Button of string * string * toolbar_cb
                  | Separator;;
let add_toolbar_buttons (toolbar : GButton.toolbar)
      (invalidator : unit -> unit)
      (iput : input_group) =
  let buttons =
    [Button ("add node",              "add-icon.png",      on_add_pressed);
     Button ("delete node",           "trash-icon.png",    on_delete_pressed);
     Separator;
     Button ("horizontal constraint", "hor-con-icon.png",  on_hor_con_pressed);
     Button ("vertical constraint",   "vert-con-icon.png", on_vert_con_pressed);
     Button ("lock constraint",       "lock-con-icon.png", on_lock_con_pressed);
     Button ("radius constraint",     "rad-con-icon.png",  on_rad_con_pressed)] in
  List.iter (fun item ->
      match item with
      | Button (label, icon, callback) ->
         let button = make_toolbar_button icon label in
         ignore (button#connect#clicked ~callback:(callback invalidator iput));
         toolbar#insert button
      | Separator ->
         let sep = GButton.separator_tool_item () in
         toolbar#insert sep)
    buttons;;

(* ==== MAIN ================================================================ *)

let () =
  ignore (GMain.init ()) in
    let w = GWindow.window ~title:"GraphKit" ~width:500 ~height:400 () in
    ignore(w#connect#destroy ~callback:GMain.quit);
    
    let vb = GPack.vbox ~packing:w#add () in
    vb#set_homogeneous false;

    (* menu bar *)
    let menubar = GMenu.menu_bar ~packing:vb#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

    (* file menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback: GMain.quit);

    (* toolbar *)
    let tb = GButton.toolbar ~packing:vb#pack () in

    (* pane *)
    let pane = GPack.paned `HORIZONTAL ~packing:(vb#add) () in
    
    (* drawing area *)
    let d = GMisc.drawing_area ~packing:(pane#pack1 ~resize:true ~shrink:false)() in
    let invalidate = d#misc#queue_draw in
    ignore(d#misc#connect#draw ~callback:draw);

    (* constraint table *)
    let model = generate_model () in
    let table_view = GTree.view ~model ~packing:(pane#pack2 ~resize:false ~shrink:true) () in

    let col = GTree.view_column ~title:"Target" ()
                ~renderer:(GTree.cell_renderer_text[], ["text", target]) in
    ignore (table_view#append_column col);
    let col = GTree.view_column ~title:"Description" ()
                ~renderer:(GTree.cell_renderer_text[], ["text", description]) in
    ignore (table_view#append_column col);

    d#set_events [`BUTTON_RELEASE; `BUTTON_PRESS; `BUTTON_MOTION ];
    ignore(d#event#connect#motion_notify ~callback:(on_mouse_move invalidate));
    ignore(d#event#connect#button_press ~callback:(on_mouse_down invalidate));
    ignore(d#event#connect#button_release ~callback:(on_mouse_up invalidate table_view));

    (* text input group *)
    let label = GMisc.label ~packing:vb#pack () in
    label#set_halign `START;
    let entry = GEdit.entry ~packing:vb#pack () in
    let (iput : input_group) = { label; entry } in

    iput.entry#set_visible false;
    ignore (iput.entry#connect#activate ~callback:(on_entry_activated iput));
    
    add_toolbar_buttons tb invalidate iput;
    w#add_accel_group accel_group;
    w#show();
    GMain.main()
