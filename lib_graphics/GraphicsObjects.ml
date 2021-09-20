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

type select_mode = None
                 | Secondary
                 | Primary

class virtual selectable_obj (id : int) (invalidate : unit -> unit) =
        object(self)
          inherit obj id invalidate
          val mutable mode = None

          method set_selection new_mode = mode <- new_mode

          method private draw (cr : Cairo.context) =
            self#draw_selection cr mode
          
          method private virtual draw_selection : Cairo.context -> select_mode -> unit
        end;;

class node (id : int) (invalidate : unit -> unit) =
object(_self)
  inherit selectable_obj id invalidate
  val size = 10.
  
  method private bounds () =
    let x, y = position in
    (x -. size /. 2., y -. size /. 2., size, size)
  
  method private draw_selection (cr : Cairo.context) (mode : select_mode) =
    let size_div_2 = size /. 2. in
    let pat = Cairo.Pattern.create_radial ~x0:(size *. 0.6) ~y0:(size *. 0.3) ~r0:1.
                ~x1:(size *. 0.6) ~y1:(size *. 0.3) ~r1:8. in
    (match mode with
     | None -> begin
         Cairo.Pattern.add_color_stop_rgb pat 0.2 0.2 0.2;
         Cairo.Pattern.add_color_stop_rgb pat ~ofs:1. 0.0 0.0 0.0;
       end
     | _ -> begin
         Cairo.Pattern.add_color_stop_rgb pat 1.0 0.2 0.2;
         Cairo.Pattern.add_color_stop_rgb pat ~ofs:1. 0.7 0.0 0.0;
       end);
    Cairo.set_source cr pat;
    (match mode with
     | Primary -> begin
         Cairo.set_line_width cr 3.;
         Cairo.rectangle cr 0. 0. ~w:(size -. 3.) ~h:(size -. 3.);
         Cairo.stroke cr;
       end
     | _ -> begin
         Cairo.arc cr size_div_2 size_div_2 ~r:size_div_2 ~a1:0. ~a2:(3.1415 *. 2.);
         Cairo.fill cr;
       end);

    (* draw text *)
    (* Cairo.set_source_rgb cr 1. 1. 1.;
     * Cairo.set_font_size cr (size *. 0.75);
     * let text_extent = Cairo.text_extents cr (Int.to_string id) in
     * Cairo.move_to cr
     *   (size *. 0.5 -. text_extent.width /. 2. -. text_extent.x_bearing)
     *   (size *. 0.5 -. text_extent.height /. 2. -. text_extent.y_bearing);
     * Cairo.show_text cr (Int.to_string id); *)
end;;
