type node = Core.Constraint.t list * Graphics.GraphicsObjects.node;;
let (nodes : node list ref) = ref [];;
let (selection : int list ref) = ref [];;

module Solver = Core.System.MakeSystem(Core.System.DirectOptimizer);;

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

let update_selection (id : int) =
  if List.exists (fun i -> i = id) !selection
  then (print_endline ("deselecting " ^ Int.to_string id);
        selection := List.filter (fun i -> not (i = id)) !selection;
        let (_, node) = List.nth !nodes id in node#deselect ())
  else (print_endline ("selecting " ^ Int.to_string id);
        selection := id::!selection;
        let (_, node) = List.nth !nodes id in node#select ());;

let on_mouse_move _invalidate event =
  let _x, _y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  true;;

let on_mouse_down _invalidate event =
  let _button = GdkEvent.Button.button event in
  let _x, _y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  true;;

let on_mouse_up invalidate event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  (* check if we clicked any nodes *)
  let clicked = List.find_opt (fun (_, g_obj) ->
                    g_obj#is_inside (x, y)) !nodes in
  (match clicked with Some (_, g_obj) -> (update_selection g_obj#get_id;
                                          invalidate ())
                    | None -> ());
  print_endline ("click at " ^ Float.to_string x ^ ", " ^ Float.to_string y);
  true;;

let draw cr =
  List.iter (fun (_, g_obj) ->
      g_obj#render cr) !nodes;
  true;;

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

let on_delete_pressed _invalidate () =
  print_endline "delete pressed";;

let on_hor_con_pressed invalidate () =
  (match !selection with
   | [] -> ()
   | [_] -> ()
   | n::ns ->
      let con = Core.Constraint.Colinear (n, Core.Constraint.Horizontal) in
      nodes := List.mapi (fun idx (cs, g_obj) ->
                   (* if node in ns, add con to its list of constraints *)
                   let cs' = 
                     if List.exists (fun i -> i = idx) ns
                     then con::cs else cs in (cs', g_obj)) !nodes);
  solve ();
  invalidate ();
  print_endline "horizontal constraint pressed";;

let on_vert_con_pressed _invalidate () =
  print_endline "vertical constraint pressed";;

let on_lock_con_pressed _invalidate () =
  print_endline "lock constraint pressed";;

let on_rad_con_pressed _invalidate () =
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

    (* drawing area *)
    let d = GMisc.drawing_area ~packing:vb#add () in
    let invalidate = d#misc#queue_draw in

    add_toolbar_buttons tb invalidate;

    d#set_events [`BUTTON_RELEASE; `BUTTON_PRESS; `BUTTON_MOTION ];
    ignore(d#event#connect#motion_notify ~callback:(on_mouse_move invalidate));
    ignore(d#event#connect#button_press ~callback:(on_mouse_down invalidate));
    ignore(d#event#connect#button_release ~callback:(on_mouse_up invalidate));
    
    ignore(d#misc#connect#draw ~callback:draw);
    
    w#add_accel_group accel_group;
    w#show();
    GMain.main()

