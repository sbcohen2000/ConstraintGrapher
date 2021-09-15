open Graph
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
  let canvas = Canvas.create [new GraphicsObjects.node invalidate] in
  let ev_parser = EventParser.create (fun ev -> Canvas.handle canvas ev) in
  ignore(w#event#connect#motion_notify ~callback:(handle_mouse_move ev_parser));
  ignore(w#event#connect#button_press ~callback:(handle_mouse_down ev_parser));
  ignore(w#event#connect#button_release ~callback:(handle_mouse_up ev_parser));
  ignore(d#misc#connect#draw ~callback:(expose d canvas));
  w#show();
  GMain.main()


