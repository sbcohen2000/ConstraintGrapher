open Graph

let nodes =
  [[ (* node 0 *)
      Core.Constraint.Point (250., 200.);
    ];[ (* node 1 *)
      Core.Constraint.Radial (0, 100.);
  ]];;

module Solver = Core.System.MakeSystem(Core.System.DirectOptimizer);;

let sys = Core.Constraint.to_system nodes;;
let sys = Array.append sys [| Core.Expression.Const 0. |];;

let init_guess = Array.make (Solver.n_vars sys) 0.;;
let soln_0 = Solver.solve sys init_guess;;
let random_guess = Array.map (fun v -> Random.float 1. +. v) init_guess;;
let soln_1 = Solver.solve sys init_guess;;

let diff = Array.map2 (fun a b -> Float.abs (a -. b) > 0.1) soln_0 soln_1;;
let _, free_vars = Array.fold_left (fun (idx, list) elem -> if elem then (idx + 1, idx::list) else (idx + 1, list)) (0, []) diff;;
print_endline (String.concat ", " (List.map Int.to_string free_vars))

let pi2 = 8. *. atan 1.

let draw canvas cr _width _height =
  Canvas.draw canvas cr

let handle_mouse_move ev_parser event =
  let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  EventParser.register_move ev_parser x y;
  true

let handle_mouse_down ev_parser event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  EventParser.register_mouse_down ev_parser x y;
  true

let handle_mouse_up ev_parser event =
  let _button = GdkEvent.Button.button event in
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  EventParser.register_mouse_up ev_parser x y;
  true

let expose drawing_area canvas cr =
  let allocation = drawing_area#misc#allocation in
  draw canvas cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true;;

let () =
  ignore (GMain.init ()) in
  let w = GWindow.window ~title:"GraphKit" ~width:500 ~height:400 () in
  ignore(w#connect#destroy ~callback:GMain.quit);
  let d = GMisc.drawing_area ~packing:w#add () in
  let invalidate = d#misc#queue_draw in
  let graphic_objects =
    List.mapi (fun i _ ->
        let gnode = new GraphicsObjects.node invalidate in
        gnode#set_position (Array.get soln_0 (2 * i), Array.get soln_0 (2 * i + 1));
        gnode) nodes in
  let canvas = Canvas.create graphic_objects in
  let ev_parser = EventParser.create (fun ev -> Canvas.handle canvas ev) in
  ignore(w#event#connect#motion_notify ~callback:(handle_mouse_move ev_parser));
  ignore(w#event#connect#button_press ~callback:(handle_mouse_down ev_parser));
  ignore(w#event#connect#button_release ~callback:(handle_mouse_up ev_parser));
  ignore(d#misc#connect#draw ~callback:(expose d canvas));
  w#show();
  GMain.main()
