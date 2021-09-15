type t = GraphicsObjects.obj list

let create (objects : GraphicsObjects.obj list) =
  objects

let draw (canvas : t) (cr : Cairo.context) =
  List.iter (fun obj -> obj#render cr) canvas

let handle (canvas : t) (ev : EventParser.event) =
  List.iter (fun obj -> obj#handle ev) canvas
