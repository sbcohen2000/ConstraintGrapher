class virtual obj (id : int) (invalidate : unit -> unit) (on_clicked : int -> unit) =
        object(self)
          val id = id
          val invalidate = invalidate
          val on_clicked = on_clicked
          
          val mutable position = (0., 0. : Geometry.Point.t)

          method set_position (pos : Geometry.Point.t) = position <- pos
          method get_position = position
          
          method render (cr : Cairo.context) =
            let px, py = Geometry.Rect.position (self#bounds ()) in
            Cairo.translate cr px py;
            self#draw cr;
            Cairo.identity_matrix cr;
            (* ~~ draw bounding box ~~ *)
            (* Cairo.set_source_rgb cr 0. 1. 0.;
             * Cairo.rectangle cr px py
             *   ~w:(Geometry.Rect.width (self#bounds ()))
             *   ~h:(Geometry.Rect.height (self#bounds ()));
             * Cairo.stroke cr; *)

          method handle (event : EventParser.event) =
            match event with
            | EventParser.Click (x, y) ->
               if Geometry.Rect.is_inside (self#bounds ()) (x, y) then
                 (on_clicked id; true)
               else false
            | EventParser.Drag _ -> false
          
          method private virtual draw : Cairo.context -> unit
          method private virtual bounds : unit -> Geometry.Rect.t
          method private virtual on_clicked : float * float -> unit
        end;;

class virtual selectable_obj (id : int) (invalidate : unit -> unit) (on_clicked : int -> unit) =
        object(self)
          inherit obj id invalidate on_clicked
          val mutable selected = false

          method select () = selected <- true
          method deselect () = selected <- false

          method private draw (cr : Cairo.context) =
            self#draw_selection cr selected
          
          method private on_clicked (_x, _y : Geometry.Point.t) =
            selected <- not selected;
            invalidate ()
          
          method private virtual draw_selection : Cairo.context -> bool -> unit
        end;;

class node (id : int) (invalidate : unit -> unit) (on_clicked : int -> unit) =
        object(_self)
          inherit selectable_obj id invalidate on_clicked
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
