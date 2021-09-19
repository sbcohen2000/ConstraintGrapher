class virtual obj (id : int) (invalidate : unit -> unit) =
        object(self)
          val id = id
          val invalidate = invalidate
          
          val mutable position = (0., 0. : Geometry.Point.t)

          method set_position (pos : Geometry.Point.t) = position <- pos
          method get_position = position
          method get_id = id
          
          method is_inside (p : Geometry.Point.t) =
            Geometry.Rect.is_inside (self#bounds ()) p
          
          method render (cr : Cairo.context) =
            let px, py = Geometry.Rect.position (self#bounds ()) in
            Cairo.translate cr px py;
            self#draw cr;
            Cairo.translate cr (-.px) (-.py);
          (* ~~ draw bounding box ~~ *)
          (* Cairo.set_source_rgb cr 0. 1. 0.;
           * Cairo.rectangle cr px py
           *   ~w:(Geometry.Rect.width (self#bounds ()))
           *   ~h:(Geometry.Rect.height (self#bounds ()));
           * Cairo.stroke cr; *)
            
          method private virtual draw : Cairo.context -> unit
          method private virtual bounds : unit -> Geometry.Rect.t
        end;;

class virtual selectable_obj (id : int) (invalidate : unit -> unit) =
        object(self)
          inherit obj id invalidate
          val mutable selected = false

          method select () = selected <- true
          method deselect () = selected <- false

          method private draw (cr : Cairo.context) =
            self#draw_selection cr selected
          
          method private virtual draw_selection : Cairo.context -> bool -> unit
        end;;

class node (id : int) (invalidate : unit -> unit) =
object(_self)
  inherit selectable_obj id invalidate
  val size = 10.
  
  method private bounds () =
    let x, y = position in
    (x, y, size, size)
  
  method private draw_selection (cr : Cairo.context) (selected : bool) =
    (* draw box *)
    Cairo.set_source_rgb cr (if selected then 1. else 0.) 0. 0.;
    Cairo.rectangle cr 0. 0. ~w:size ~h:size;
    Cairo.fill cr;

    (* draw text *)
    Cairo.set_source_rgb cr 1. 1. 1.;
    Cairo.set_font_size cr (size *. 0.75);
    let text_extent = Cairo.text_extents cr (Int.to_string id) in
    Cairo.move_to cr
      (size *. 0.5 -. text_extent.width /. 2. -. text_extent.x_bearing)
      (size *. 0.5 -. text_extent.height /. 2. -. text_extent.y_bearing);
    Cairo.show_text cr (Int.to_string id);
end;;
