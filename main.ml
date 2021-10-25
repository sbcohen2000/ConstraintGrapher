open Geometry.Primitives;;

Gc.set {(Gc.get ()) with Gc.verbose = 0x01};;

type select_mode = Unselected
                 | Secondary
                 | Primary;;

type node = { constraints : Core.Constraint.t list;
              select_state : select_mode;
              position : Point.t;
              id : int };;

let node_bounds (n : node) =
  let x, y = n.position
  and size = 10. in
  (x -. size /. 2., y -. size /. 2., size, size);;

let (nodes : node list ref) = ref [];;
let (selection : int list ref) = ref [];;

type drawing = Line of { a : int;
                         b : int }
             | Circle of { c : int;
                           r : int };;
let (drawings : drawing list ref) = ref [];;

module Solver = Core.System.MakeSystem(Core.System.GradientOptimizer);;

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

let rec last lst =
  match lst with
  | [] -> None
  | [n] -> Some n
  | _::ns -> last ns;;

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

let set_message (iput : input_group) (msg : string) =
  iput.label#set_text msg;;

let register_action (act : action) (iput : input_group) =
  pending_action := Some act;
  set_message iput act.instruction;
  iput.entry#set_text "";
  iput.entry#set_visible true;
  iput.entry#set_has_focus true;;

let on_entry_activated (iput : input_group) () =
  let act_opt = !pending_action in
  pending_action := None;
  match act_opt with
  | None -> ()
  | Some act ->
     iput.entry#set_visible false;
     let str = iput.entry#text in
     let succ = act.f str in
     if succ then ()
     else set_message iput
            (act.instruction ^ " invalid input \"" ^ str ^ "\"");;

(* ==== CONSTRAINT TABLE ==================================================== *)

let c_table_cols = new GTree.column_list;;
let target = c_table_cols#add Gobject.Data.string_option;;
let description = c_table_cols#add Gobject.Data.string;;

let generate_model () =
  let store = GTree.tree_store c_table_cols in
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

(* ==== TABLE CONTROLS ====================================================== *)

let rec remove_nth (lst : 'a list) (n : int) =
  match lst with
  | [] -> []
  | o::os -> if n = 0 then os
             else o::(remove_nth os (n - 1))

(* deletes constraint n from the currently selected node.
 * If no node is selected, the function does nothing *)
let delete_con (n : int) =
  let node_id = last !selection in
  (match node_id with
   | Some id ->
      nodes := List.map (fun node ->
                   let cs' = 
                     if id = node.id then
                       remove_nth node.constraints n
                     else node.constraints in
                   { node with constraints = cs' }) !nodes
   | None -> ());;

let on_key_pressed_in_table (tb : GTree.view) (ev : GdkEvent.Key.t) =
  match GdkEvent.Key.keyval ev with
  | 65288 ->
     (match tb#get_cursor () with
      | (Some path, _) ->
         let indices = GtkTree.TreePath.get_indices path in
         let con_to_delete = Array.get indices 0 in
         delete_con con_to_delete;
         set_model tb;
      | _ -> ());
     true;
  | _ -> false;;

(* ==== GEOMETRIC SOLVING =================================================== *)

let render_drawings cr =
  Cairo.set_source_rgb cr 0. 0. 0.;
  List.iter (fun drawing ->
      match drawing with
      | Line l ->
         let node_a = node_n l.a in
         let node_b = node_n l.b in
         let ax, ay = node_a.position in
         let bx, by = node_b.position in
         Cairo.set_line_width cr 3.;
         Cairo.move_to cr ax ay;
         Cairo.line_to cr bx by;
         Cairo.stroke cr;
      | Circle c ->
         let node_c = node_n c.c in
         let node_r = node_n c.r in
         let ax, ay = node_c.position in
         let b = node_r.position in
         let r = Geometry.Primitives.Point.distance (ax, ay) b in
         Cairo.set_line_width cr 3.;
         Cairo.arc cr ax ay ~r ~a1:0.0 ~a2:6.283185;
         Cairo.stroke cr;
    ) !drawings;;

let render_node cr (n : node) =
  let bounds = node_bounds n in
  let px, py, _w, _h = bounds in
  Cairo.translate cr px py;
  let size = 10. in
  let size_div_2 = size /. 2. in
    let pat = Cairo.Pattern.create_radial ~x0:(size *. 0.6) ~y0:(size *. 0.3) ~r0:1.
                ~x1:(size *. 0.6) ~y1:(size *. 0.3) ~r1:8. in
    (match n.select_state with
     | Unselected -> begin
         Cairo.Pattern.add_color_stop_rgb pat 0.2 0.2 0.2;
         Cairo.Pattern.add_color_stop_rgb pat ~ofs:1. 0.0 0.0 0.0;
       end
     | _ -> begin
         Cairo.Pattern.add_color_stop_rgb pat 1.0 0.2 0.2;
         Cairo.Pattern.add_color_stop_rgb pat ~ofs:1. 0.7 0.0 0.0;
       end);
    Cairo.set_source cr pat;
    (match n.select_state with
     | Primary -> begin
         Cairo.set_line_width cr 3.;
         Cairo.rectangle cr (1.5) (1.5) ~w:(size -. 3.) ~h:(size -. 3.);
         Cairo.stroke cr;
       end
     | _ -> begin
         Cairo.arc cr size_div_2 size_div_2 ~r:size_div_2 ~a1:0. ~a2:(3.1415 *. 2.);
         Cairo.fill cr;
       end);
    (match None with
     | Some s -> begin
         Cairo.set_source_rgb cr 0.0 0.0 0.0;
         Cairo.set_font_size cr 10.;
         let te = Cairo.text_extents cr s in
         Cairo.move_to cr (0.5 -. te.width -. te.x_bearing)
           (0.5 -. te.height -. te.y_bearing);
         Cairo.show_text cr s;
       end
     | None -> ());
    Cairo.translate cr (-.px) (-.py);;

let draw cr (w : float) (h : float) =
  (* white background *)
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  let primary = last !selection in
  let grid_offset = match primary with
    | Some id ->
       let node = node_n id in
       node.position
    | None -> Point.zero in
  Geometry.Grid.draw cr w h grid_offset;
  render_drawings cr;
  List.iter (render_node cr) !nodes;;

let relative_con (con : Core.Constraint.t) =
  match con with
  | Core.Constraint.Axis (targ, dir) ->
     Core.Constraint.Axis (relative_id targ, dir)
  | Core.Constraint.Point (x, y) ->
     Core.Constraint.Point (x, y)
  | Core.Constraint.Radial (targ, f) ->
     Core.Constraint.Radial (relative_id targ, f)
  | Core.Constraint.Offset (targ, x, y) ->
     Core.Constraint.Offset (relative_id targ, x, y)
  | Core.Constraint.Colinear (targ_a, targ_b) ->
     Core.Constraint.Colinear (relative_id targ_a,
                               relative_id targ_b)

let relative_cons (cons : Core.Constraint.t list) =
  List.map relative_con cons

let ordered_relative_constraints () =
  let sorted_nodes = List.sort
                       (fun a b -> Int.compare a.id b.id)
                       !nodes in
  List.fold_right (fun node cs ->
      let r_cons = relative_cons node.constraints in
      r_cons::cs) sorted_nodes [];;

let system_vector () =
  let size = 2 * (List.length !nodes) in
  let vect = Array.create_float size in
  List.iter (fun node ->
      let x, y = node.position in
      let rel_id = relative_id node.id in
      Array.set vect (rel_id * 2) x;
      Array.set vect (rel_id * 2 + 1) y) !nodes;
  vect

let solve () =
  let cs = ordered_relative_constraints () in
  List.iteri (fun idx cs -> print_endline (Int.to_string idx ^ ":");
                            List.iter (fun con ->
                                print_endline ("\t" ^ (Core.Constraint.to_string con)))
                              cs)
    cs;
  let system = Core.Constraint.to_system cs in
  let init_guess = system_vector () in
  let soln = Solver.solve system init_guess in
  nodes := List.map (fun node ->
               let rel_id = relative_id node.id in
               let position = (Array.get soln (2 * rel_id),
                               Array.get soln (2 * rel_id + 1)) in
               { node with position; }) !nodes;;

let create_solution_updater invalidate (d1, d2 : int * int) =
  let cs = ordered_relative_constraints () in
  let system = Core.Constraint.to_system cs in
  let soln = ref (system_vector ()) in
  let render = fun soln ->
    nodes := List.map (fun node ->
                 let rel_id = relative_id node.id in
                 let position = (Array.get soln (2 * rel_id),
                                 Array.get soln (2 * rel_id + 1)) in
                 { node with position; }) !nodes; invalidate () in
  fun (tx, ty : float * float) ->
  let rec iter = fun last_soln ->
    (* need to accumulate a few steps for the delta
     * measurement to be effective *)
    for _ = 0 to 10 do
      soln := Solver.step_solution system !soln (d1, d2) (tx, ty);
    done;
    render !soln;
    let delta = Array.fold_left (fun d elem -> d +. Float.abs elem) 0.0
                  (Array.map2 (fun a b -> a -. b) !soln last_soln) in
    (* if the system has reached steady state, we can stop *)
    if delta < 1. then ()
    else iter !soln in
  iter !soln;
  render !soln;;

(* remove labels from all nodes *)
let unlabel_all () =
  List.iter (fun _node -> ()) !nodes;;

(* label all of the nodes constrained with node n *)
let label_constraints (n : node) =
  List.iter (fun con ->
      List.iter (fun id -> 
         let _targ_node = node_n id in ()
        (* (targ_node.g_obj#set_label (Int.to_string id) *))
        (Core.Constraint.targets con))
    n.constraints;;

let label_primary_selection () =
  unlabel_all ();
  let primary_id = last !selection in
  match primary_id with
  | Some id ->
     let primary = node_n id in
     label_constraints primary;
  | None -> ()

let update_selection (table_view : GTree.view) =
  let fst = List.nth_opt !selection 0 in
  nodes := List.map (fun node ->
               let select_state = match fst with
                 | Some n -> if node.id = n then Primary
                             else if List.exists (fun n -> n = node.id) !selection then
                               Secondary
                             else
                               Unselected
                 | None -> Unselected in
               { node with select_state; }
             ) !nodes;
  label_primary_selection ();
  set_model table_view;;

(* ==== MOUSE CONTROLS ====================================================== *)

(* returns the node id and the ordered id
 * of the clicked node *)
let node_underneath (x, y : float * float) =
  let sorted_nodes = List.sort (fun a b -> Int.compare a.id b.id) !nodes in
  let rec find = fun i ns ->
    match ns with
    | [] -> None
    | n::ns -> if Rect.is_inside (node_bounds n) (x, y) then Some (n.id, i)
               else find (i + 1) ns in
  find 0 sorted_nodes;;

type updater = float * float -> unit
type drag = Pending of updater (* clicked down, not dragged yet *)
          | Valid of float * float * updater  (* dragging *)
          | Invalid;; (* clicked on nothing *)
let current_drag = ref Invalid;;

let on_mouse_move invalidate event =
  let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  (match !current_drag with
   | Pending upd -> current_drag := Valid (x, y, upd)
   | Valid (_, _, upd) ->
      upd (x, y);
      invalidate ();
   | Invalid -> ());
  true;;

let on_mouse_down invalidate event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  let clicked = node_underneath (x, y) in
  (match clicked with
   | Some (_, ordered_id) ->
      let x_dim = ordered_id * 2 in
      let y_dim = ordered_id * 2 + 1 in
      let upd = create_solution_updater invalidate (x_dim, y_dim) in
      current_drag := Pending upd
   | None -> current_drag := Invalid);
  true;;

let on_mouse_up invalidate table_view event =
  let _button = GdkEvent.Button.button event in
  let shift_on = (GdkEvent.Button.state event) land 1 = 1 in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  (* check if we clicked any nodes *)
  let clicked = node_underneath (x, y) in
  (match clicked, !current_drag with
   | (Some (id, _), Valid (sx, sy, _)) ->
      let d = Point.distance (x, y) (sx, sy) in
      (* if the user dragged for fewer than 2 units, treat it as a click *)
      (if shift_on then (if d < 2. then selection := !selection @ [id])
       else selection := [id];
       update_selection table_view)
   | (Some (id, _), _) ->
      (if shift_on then (selection := !selection @ [id])
       else selection := [id];
       update_selection table_view)
   | (None, Valid _) -> ()
   | (None, _) -> selection := [];
                  update_selection table_view);
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
  let new_node = {
      constraints = [];
      position = Random.float 250., Random.float 200.;
      select_state = Unselected;
      id = new_id (); } in
  nodes := new_node::!nodes;
  invalidate ();
  print_endline "added node";;

(* add a constraint between just two nodes *)
let add_binary_constraint (apply_con : int -> Core.Constraint.t) =
  match !selection with
  | [a; b] ->
     let con = apply_con b in
     nodes := List.map (fun node ->
                  let cs' = if node.id = a
                            then con::node.constraints
                            else node.constraints in
                  { node with constraints = cs' }) !nodes
  | _ -> ();;

(* add a constraint between the primary node and 
 * every selected node *)
let add_constraints_to_selected (apply_con : int -> Core.Constraint.t) =
  match !selection with
  | [] -> ()
  | [_] -> ()
  | n::ns ->
     let con = apply_con n in
     nodes := List.map (fun node ->
                  (* if node in ns, add con to its list of constraints *)
                  let cs' =
                    if List.exists (fun sel -> sel = node.id) ns
                    then con::node.constraints
                    else node.constraints in
                  { node with constraints = cs' }) !nodes

let on_delete_pressed invalidate _iput () =
  let delete_node = fun id ->
    (* delete the node from the list of nodes *)
    nodes := List.filter (fun node -> not (node.id = id)) !nodes;
    
    (* delete every constraint that references the deleted node *)
    nodes := List.map (fun node ->
                 let cs' = List.filter (fun con ->
                               let targs = Core.Constraint.targets con in
                               not (List.exists (fun targ -> targ = id) targs))
                             node.constraints in
                 { node with constraints = cs' }) !nodes; in
  List.iter delete_node !selection;
  selection := [];
  label_primary_selection ();
  invalidate ();;

let on_hor_con_pressed invalidate _iput () =
  add_constraints_to_selected (fun n -> Core.Constraint.Axis (n, Core.Constraint.Horizontal));
  solve ();
  invalidate ();;

let on_vert_con_pressed invalidate _iput () =
  add_constraints_to_selected (fun n -> Core.Constraint.Axis (n, Core.Constraint.Vertical));
  solve ();
  invalidate ();;

let on_lock_con_pressed _invalidate _iput () =
  nodes := List.map (fun node ->
               let cs' = if List.exists (fun sel -> sel = node.id) !selection
                         then let x, y = node.position in
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
  match !selection with
  | [] -> () (* if there is no selection, don't register an action *)
  | _ -> 
     register_action { f = action;
                       instruction = "radius:" } iput;;

let on_offset_con_pressed invalidate (iput : input_group) () =
  let the_x = ref None in
  let get_vert_offset = (fun str ->
      let f = Float.of_string_opt str in
      match (!the_x, f) with
      | (Some x, Some y) ->
         add_binary_constraint (fun n -> Core.Constraint.Offset (n, x, -.y));
         solve ();
         invalidate ();
         true
      | _ -> false) in
  let get_hor_offset = (fun str ->
      let f = Float.of_string_opt str in
      match f with
      | Some x ->
         begin
           the_x := Some x;
           register_action { f = get_vert_offset;
                             instruction = "vertical offset:" } iput;
           true
         end
      | None -> false) in
  match !selection with
  | [] -> ()
  | _ ->
     register_action { f = get_hor_offset;
                       instruction = "horizontal offset:" } iput;;

let on_colinear_con_pressed invalidate (iput : input_group) () =
  match !selection with
  | [a; b; prim] ->
     let con = Core.Constraint.Colinear (a, b) in
     nodes := List.map (fun node ->
                  let cs' = if node.id = prim
                            then con::node.constraints
                            else node.constraints in
                  { node with constraints = cs' }) !nodes;
     solve ();
     invalidate ();
  | _ ->
     set_message iput "Colinear constriant requires three nodes";;

let on_line_pressed invalidate (_iput : input_group) () =
  match !selection with
  | [a; b] -> 
     let (new_line : drawing) = Line { a; b } in
     drawings := new_line::!drawings;
     invalidate ();
  | _ -> ();;
  
let on_circle_pressed invalidate (_iput : input_group) () =
    match !selection with
  | [r; c] -> 
     let (new_circle : drawing) = Circle { c; r } in
     drawings := new_circle::!drawings;
     invalidate ();
  | _ -> ();;

let make_toolbar_button (image_name : string) (label : string) =
  let path = "./resources/" ^ image_name in
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
    [Button ("add node",    "add-icon.png",   on_add_pressed);
     Button ("delete node", "trash-icon.png", on_delete_pressed);
     Separator;
     Button ("horizontal constraint", "hor-con-icon.png",      on_hor_con_pressed);
     Button ("vertical constraint",   "vert-con-icon.png",     on_vert_con_pressed);
     Button ("lock constraint",       "lock-con-icon.png",     on_lock_con_pressed);
     Button ("radius constraint",     "rad-con-icon.png",      on_rad_con_pressed);
     Button ("offset constraint",     "offset-con-icon.png",   on_offset_con_pressed);
     Button ("colinear constraint",   "colinear-con-icon.png", on_colinear_con_pressed);
     Separator;
     Button ("draw line", "line-icon.png", on_line_pressed);
     Button ("draw circle", "circle-icon.png", on_circle_pressed)] in
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

let expose drawing_area cr =
  let allocation = drawing_area#misc#allocation in
  draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true;;

let () =
  ignore (GMain.init ()) in
    let w = GWindow.window ~title:"Constraint Grapher" ~width:700 ~height:600 () in
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
    ignore(d#misc#connect#draw ~callback:(expose d));

    (* constraint table *)
    let model = generate_model () in
    let table_view = GTree.view ~model ~packing:(pane#pack2 ~resize:false ~shrink:true) () in
    table_view#set_enable_search false;
    ignore (table_view#event#connect#key_release ~callback:(on_key_pressed_in_table table_view));

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
