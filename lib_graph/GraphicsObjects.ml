class virtual obj (invalidate : unit -> unit) =
        object(self)
          val invalidate = invalidate
          val mutable position = (0., 0. : Geometry.Point.t)
          
          method render (cr : Cairo.context) =
            let px, py = Geometry.Rect.position (self#bounds ()) in
            Cairo.translate cr px py;
            self#draw cr;

          method handle (event : EventParser.event) =
            match event with
            | EventParser.CLICK (x, y) ->
               if Geometry.Rect.is_inside (self#bounds ()) (x, y) then
                 self#on_clicked (x, y)
            | EventParser.DRAG _ -> ()
          
          method private virtual draw : Cairo.context -> unit
          method private virtual bounds : unit -> Geometry.Rect.t
          method private virtual on_clicked : float * float -> unit
        end;;

class node (invalidate : unit -> unit) =
        object(_self)
          inherit obj (invalidate)

          method private bounds () =
            let x, y = position in
            (x, y, 10., 10.)
          
          method private draw (cr : Cairo.context) =
            Cairo.set_source_rgb cr 0. 0. 0.;
            Cairo.rectangle cr 0. 0. ~w:10. ~h:10.;
            Cairo.fill cr;

          method private on_clicked (_x, _y : Geometry.Point.t) =
            print_endline "Clicked me!"
        end;;
