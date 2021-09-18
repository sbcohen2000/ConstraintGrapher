open Graphics

let draw file cr _width _height =
  App.File.draw file cr

let handle_mouse_move ev_parser event =
  let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
  EventParser.register_move ev_parser x y;
  true;;

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

let expose drawing_area file cr =
  let allocation = drawing_area#misc#allocation in
  draw file cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true;;

let () =
  ignore (GMain.init ()) in
    let w = GWindow.window ~title:"GraphKit" ~width:500 ~height:400 () in
    ignore(w#connect#destroy ~callback:GMain.quit);
    let d = GMisc.drawing_area ~packing:w#add () in
    let invalidator = d#misc#queue_draw in
    let file = App.File.create invalidator in
    let id1 = App.File.add_node file in
    let id2 = App.File.add_node file in
    App.File.add_constraint file id1 (Core.Constraint.Point (250., 250.));
    App.File.add_constraint file id2 (Core.Constraint.Radial (id1, 100.));
    let ev_parser = EventParser.create (fun ev -> App.File.handle file ev) in
    ignore(w#event#connect#motion_notify ~callback:(handle_mouse_move ev_parser));
    ignore(w#event#connect#button_press ~callback:(handle_mouse_down ev_parser));
    ignore(w#event#connect#button_release ~callback:(handle_mouse_up ev_parser));
    ignore(d#misc#connect#draw ~callback:(expose d file));
    w#show();
    GMain.main()

