type node = { constraints : Core.Constraint.t list;
              g_obj : Graphics.GraphicsObjects.node;
              id : int };;
let (nodes : node list ref) = ref [];;
let (selection : int list ref) = ref [];;

module Solver = Core.System.MakeSystem(Core.System.DirectOptimizer);;

let node_n (id : int) =
  List.find (fun node -> id = node.id) !nodes

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

let system_vector () =
  let size = 2 * (List.length !nodes) in
  let vect = Array.create_float size in
  List.iter (fun node ->
      let x, y = node.g_obj#get_position in
      Array.set vect (node.id * 2) x;
      Array.set vect (node.id * 2 + 1) y) !nodes;
  vect

let ordered_constraints () =
  let sorted_nodes = List.sort
                       (fun a b -> Int.compare a.id b.id)
                       !nodes in
  List.fold_right (fun node cs ->
      node.constraints::cs) sorted_nodes []

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
               node.g_obj#set_position (Array.get soln (2 * node.id),
                                        Array.get soln (2 * node.id + 1)))
    !nodes;;

let create_solution_updater (dim : int) =
  let cs = ordered_constraints () in
  let system = Core.Constraint.to_system cs in
  fun (target : float) -> 
  let init_guess = system_vector () in
  let soln = Solver.vary_solution system init_guess dim target in
  List.iter (fun node ->
      node.g_obj#set_position (Array.get soln (2 * node.id),
                               Array.get soln (2 * node.id + 1)))
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

let node_underneath (x, y : float * float) =
  List.find_opt (fun node ->
                    node.g_obj#is_inside (x, y)) !nodes

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
   | Some node ->
      let x_dim = node.id * 2 in
      let y_dim = node.id * 2 + 1 in
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
   | (Some node, Valid (sx, sy, _)) ->
      let d = Graphics.Geometry.Point.distance (x, y) (sx, sy) in
      (* if the user dragged for fewer than 2 units, treat it as a click *)
      if d < 2. then
        update_selection node.id table_view
      else ()
   | (Some node, _) ->
      update_selection node.id table_view
   | (None, Valid _) -> ()
   | (None, _) -> deselect_all table_view);
  current_drag := Invalid;
  invalidate ();
  true;;

(* ==== TOOLBAR BUTTONS ===================================================== *)

let on_add_pressed invalidate () =
  let id = List.length !nodes in
  let g_obj = new Graphics.GraphicsObjects.node () in
  g_obj#set_position (Random.float 250., Random.float 200.);
  let new_node = {
      constraints = [];
      g_obj; id; } in
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
     let con = apply_con n in
     nodes := List.map (fun node ->
                  (* if node in ns, add con to its list of constraints *)
                  let cs' =
                    if List.exists (fun sel -> sel = node.id) ns
                    then con::node.constraints
                    else node.constraints in
                  { node with constraints = cs' }) !nodes

let on_delete_pressed _invalidate () =
  print_endline "not implemented";;

let on_hor_con_pressed invalidate () =
  add_constraints_to_selected (fun n -> Core.Constraint.Colinear (n, Core.Constraint.Horizontal));
  solve ();
  invalidate ();
  print_endline "horizontal constraint pressed";;

let on_vert_con_pressed invalidate () =
  add_constraints_to_selected (fun n -> Core.Constraint.Colinear (n, Core.Constraint.Vertical));
  solve ();
  invalidate ();
  print_endline "vertical constraint pressed";;

let on_lock_con_pressed _invalidate () =
  nodes := List.map (fun node ->
               let cs' = if List.exists (fun sel -> sel = node.id) !selection
                         then let x, y = node.g_obj#get_position in
                              let con = Core.Constraint.Point (x, y) in
                              con::node.constraints
                         else node.constraints in
             { node with constraints = cs' }) !nodes

let on_rad_con_pressed invalidate () =
  add_constraints_to_selected (fun n -> Core.Constraint.Radial (n, 100.));
  solve ();
  invalidate ();
  print_endline "radial constraint pressed";;

let make_toolbar_button (image_name : string) (label : string) =
  let path = "/home/sam/Documents/ConstraintGrapher/resources/" ^ image_name in
  let image = GMisc.image ~file:path () in
  let button = GButton.tool_button ~label ~use_underline:true () in
  button#set_icon_widget image#coerce;
  button;;

type toolbar_cb = (unit -> unit) -> unit -> unit;;
type toolbar_item = Button of string * string * toolbar_cb
                  | Separator;;
let add_toolbar_buttons (toolbar : GButton.toolbar) (invalidator : unit -> unit) =
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
         ignore (button#connect#clicked ~callback:(callback invalidator));
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
    add_toolbar_buttons tb invalidate;
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

    (* text entry area *)
    let entry = GEdit.entry ~packing:vb#pack () in
    entry#set_has_focus false;
    
    w#add_accel_group accel_group;
    w#show();
    GMain.main()
