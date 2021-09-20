type node = Core.Constraint.t list * Graphics.GraphicsObjects.node;;
let (nodes : node list ref) = ref [];;
let (selection : int list ref) = ref [];;

module Solver = Core.System.MakeSystem(Core.System.DirectOptimizer);;

(* ==== CONSTRAINT TABLE ==================================================== *)

let c_table_cols = new GTree.column_list;;
let target = c_table_cols#add Gobject.Data.string_option;;
let description = c_table_cols#add Gobject.Data.string;;

let update_model () =
  let store = GTree.tree_store c_table_cols in
  let rec last = fun lst ->
    match lst with
    | [] -> None
    | [n] -> Some n
    | _::ns -> last ns in
  let selected_node = last !selection in
  let cs = match selected_node with
    | None -> [] 
    | Some n -> let (cs, _) = List.nth !nodes n in cs in
  List.iter (fun con ->
      let row = store#append () in
      store#set ~row ~column:target
        (Core.Constraint.target_string_opt con);
      store#set ~row ~column:description
        (Core.Constraint.description con)) cs;
  store;;

let set_model (table_view : GTree.view) =
  let model = update_model () in
  let model = model#coerce in
  table_view#set_model (Some model);;

(* ==== SOLUTION SOLVING ==================================================== *)

let draw cr =
  List.iter (fun (_, g_obj) ->
      g_obj#render cr) !nodes;
  true;;

let system_vector (g_objs : Graphics.GraphicsObjects.node list) =
  let positions = List.map (fun g_obj ->
                      let x, y = g_obj#get_position in
                      [| x; y |]) g_objs in
  Array.concat positions

let solve () =
  let cs, g_objs = List.fold_right (fun (c, g_obj) (cs, g_objs) ->
                       (c::cs, g_obj::g_objs)) !nodes ([], []) in
  List.iteri (fun idx cs -> print_endline (Int.to_string idx ^ ":");
                            List.iter (fun con ->
                                print_endline ("\t" ^ (Core.Constraint.to_string con)))
                              cs)
    cs;
  let system = Core.Constraint.to_system cs in
  Array.iter (fun expr -> print_endline (Core.Expression.to_string expr)) system;
  let init_guess = system_vector g_objs in
  let soln = Solver.solve system init_guess in
  List.iteri (fun idx g_obj ->
               g_obj#set_position (Array.get soln (2 * idx),
                                   Array.get soln (2 * idx + 1)))
    g_objs;;

let create_solution_updater (dim : int) =
  let cs, g_objs = List.fold_right (fun (c, g_obj) (cs, g_objs) ->
                       (c::cs, g_obj::g_objs)) !nodes ([], []) in
  let system = Core.Constraint.to_system cs in
  fun (target : float) -> 
  let init_guess = system_vector g_objs in
  let soln = Solver.vary_solution system init_guess dim target in
  List.iteri (fun idx g_obj ->
      g_obj#set_position (Array.get soln (2 * idx),
                          Array.get soln (2 * idx + 1)))
    g_objs;;

let update_selection (id : int) (table_view : GTree.view) =
  if List.exists (fun i -> i = id) !selection
  then (print_endline ("deselecting " ^ Int.to_string id);
        selection := List.filter (fun i -> not (i = id)) !selection;
        let (_, node) = List.nth !nodes id in
        node#set_selection Graphics.GraphicsObjects.None)
  else (print_endline ("selecting " ^ Int.to_string id);
        match !selection with
        | [] -> 
           selection := id::!selection;
           let (_, node) = List.nth !nodes id in
           node#set_selection Graphics.GraphicsObjects.Primary;
        | _ ->
           selection := id::!selection;
           let (_, node) = List.nth !nodes id in
           node#set_selection Graphics.GraphicsObjects.Secondary);
  set_model table_view;;

let deselect_all (table_view : GTree.view) =
  print_endline "deselecting all";
  selection := [];
  List.iter (fun (_, g_obj) -> g_obj#set_selection Graphics.GraphicsObjects.None) !nodes;
  set_model table_view;;

(* ==== MOUSE CONTROLS ====================================================== *)

let node_underneath (x, y : float * float) =
  List.find_opt (fun (_, g_obj) ->
                    g_obj#is_inside (x, y)) !nodes

type updaters = { x_updater : float -> unit;
                  y_updater : float -> unit }
type drag = Pending of updaters (* clicked down, not dragged yet *)
          | Valid of updaters (* dragging *)
          | Invalid;; (* clicked on nothing *)
let current_drag = ref Invalid;;

let on_mouse_move invalidate event =
  let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  (match !current_drag with
   | Pending upd -> current_drag := Valid upd
   | Valid upd ->
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
   | Some (_, g_obj) ->
      let x_dim = g_obj#get_id * 2 in
      let y_dim = g_obj#get_id * 2 + 1 in
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
  (match clicked with
   | Some (_, g_obj) ->
      (* if the user just dragged, don't register
       * a selection when they release the mouse *)
      (match !current_drag with
       | Valid _ -> ()
       | _ -> update_selection g_obj#get_id table_view)
   | None -> deselect_all table_view);
  current_drag := Invalid;
  invalidate ();
  print_endline ("click at " ^ Float.to_string x ^ ", " ^ Float.to_string y);
  true;;

(* ==== TOOLBAR BUTTONS ===================================================== *)

let make_toolbar_button (image_name : string) (label : string) =
  let path = "/home/sam/Documents/ConstraintGrapher/resources/" ^ image_name in
  let image = GMisc.image ~file:path () in
  let button = GButton.tool_button ~label ~use_underline:true () in
  button#set_icon_widget image#coerce;
  button;;

let on_add_pressed invalidate () =
  let new_id = List.length !nodes in
  let g_obj = new Graphics.GraphicsObjects.node new_id (fun () -> ()) in
  g_obj#set_position (Random.float 250., Random.float 200.);
  nodes := !nodes @ [([], g_obj)];
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
     nodes := List.mapi (fun idx (cs, g_obj) ->
                  (* if node in ns, add con to its list of constraints *)
                  let cs' = 
                    if List.exists (fun i -> i = idx) ns
                    then con::cs else cs in (cs', g_obj)) !nodes

let on_delete_pressed _invalidate () =
  print_endline "delete pressed";;

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
  print_endline "lock constraint pressed";;

let on_rad_con_pressed invalidate () =
  add_constraints_to_selected (fun n -> Core.Constraint.Radial (n, 100.));
  solve ();
  invalidate ();
  print_endline "radial constraint pressed";;

let add_toolbar_buttons (toolbar : GButton.toolbar) (invalidator : unit -> unit) =
  let buttons =
    [("add node",              "add-icon.png",      on_add_pressed);
     ("delete node",           "trash-icon.png",    on_delete_pressed);
     ("horizontal constraint", "hor-con-icon.png",  on_hor_con_pressed);
     ("vertical constraint",   "vert-con-icon.png", on_vert_con_pressed);
     ("lock constraint",       "lock-con-icon.png", on_lock_con_pressed);
     ("radius constraint",     "rad-con-icon.png",  on_rad_con_pressed)] in
  List.iter (fun (label, icon, callback) ->
      let button = make_toolbar_button icon label in
      ignore (button#connect#clicked ~callback:(callback invalidator));
      toolbar#insert button)
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
    let model = update_model () in
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
    
    w#add_accel_group accel_group;
    w#show();
    GMain.main()

