open Primitives

let draw (cr: Cairo.context) (w : float) (h : float)
      (offset : Point.t) =
  let (offset_x, offset_y) = offset in
  let size = 100. in
  let grid_offset_x = Float.to_int (Float.floor (offset_x /. size)) in
  let grid_offset_y = Float.to_int (Float.floor (offset_y /. size)) in
  let sub_grid_offset_x = Float.rem offset_x size in
  let sub_grid_offset_y = Float.rem offset_y size in
  let text_padding = 3. in
  let n_vert_lines = Float.to_int (Float.floor (w /. size)) in
  let n_horz_lines = Float.to_int (Float.floor (h /. size)) in
  begin
    Cairo.set_line_width cr 0.5;
    Cairo.set_source_rgb cr 0.7 0.7 0.7;
    Cairo.set_font_size cr 8.;
    
    (* draw vertical lines *)
    for k = 0 to n_vert_lines do
      let x = Float.of_int k *. size +. sub_grid_offset_x in
      let str = Int.to_string (Float.to_int size * (k - grid_offset_x)) in
      let te = Cairo.text_extents cr str in
      Cairo.move_to cr x 0.;
      Cairo.line_to cr x (h -. (te.width +. text_padding *. 2.));
      Cairo.stroke cr;
      
      (* draw label *)
      Cairo.move_to cr (x -. te.height /. 2. -. te.y_bearing)
        (h -. text_padding);
      Cairo.save cr;
      Cairo.rotate cr (-3.1415 /. 2.);
      Cairo.show_text cr str;
      Cairo.restore cr;
    done;

    (* draw horizontal lines *)
    for k = 0 to n_horz_lines do
      let y = Float.of_int k *. size +. sub_grid_offset_y in
      let str = Int.to_string (Float.to_int size * (-k + grid_offset_y)) in
      let te = Cairo.text_extents cr str in
      Cairo.move_to cr (te.width +. text_padding *. 2.) y;
      Cairo.line_to cr w y;
      Cairo.stroke cr;

      (* draw label *)
      Cairo.move_to cr text_padding (y -. te.height /. 2. -. te.y_bearing);
      Cairo.show_text cr str;
    done;
    Cairo.stroke cr;
  end;;
