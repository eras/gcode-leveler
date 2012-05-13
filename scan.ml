open BatPervasives
open BatStd

module Vector =
struct
  include Vector
  include Vector.Ops
end

type vector = (float * float)

let vector ((x : float), (y : float)) = (x, y)

let pi = atan 1.0 *. 4.0

let x_of_vec ((x, y) : vector) = x

let y_of_vec ((x, y) : vector) = y

let ab_length (a, b) = int_of_float (Vector.(length (a -| b)))

let incr_wrap max x =
  incr x;
  if !x >= max then
    x := !x - max

let wrap_range max x =
  let mx = x mod max in
  if mx < 0 
  then max + mx
  else mx

let local_maximas ~limit xs window_size = 
  let module FloatMap = BatMap.Make(BatFloat) in
  let window_contents = Array.make window_size 0.0 in
  let window_map = ref FloatMap.empty in
  let window_add_ofs = ref 0 in
  let window_remove_ofs = ref 0 in
  let window_num_els = ref 0 in
  let remove () =
    let x = window_contents.(!window_remove_ofs) in
      (* Printf.printf "Removing %f at %d\n" x !window_remove_ofs; *)
      incr_wrap window_size window_remove_ofs;
      decr window_num_els;
      let v = FloatMap.find x !window_map in
	decr v;
	if !v = 0 then
	  window_map := FloatMap.remove x !window_map
  in
  let add x =
    if !window_num_els > 0 && !window_add_ofs = !window_remove_ofs then
      remove ();
    (* Printf.printf "Adding %f at %d\n" x !window_add_ofs; *)
    window_contents.(!window_add_ofs) <- x;
    incr_wrap window_size window_add_ofs;
    incr window_num_els;
    try incr (FloatMap.find x !window_map)
    with Not_found -> window_map := FloatMap.add x (ref 1) !window_map
  in 
  let wrap_xs = wrap_range (Array.length xs) in
    for c = 0 to window_size - 1 do
      add xs.(wrap_xs (c - window_size / 2));
    done;
    let maximas = ref [] in
      for x_at = 0 to Array.length xs - 1 do 
	let (range_max, _) = FloatMap.max_binding !window_map in
	  (* Printf.printf "%d %f\n" x_at range_max; *)
	  if xs.(x_at) = range_max && limit xs.(x_at) then
	    maximas := x_at ::!maximas;
	  add xs.(wrap_xs (x_at + window_size / 2));
      done;
      !maximas

let nth_step ((v0, v1), n) m =
  let open Vector in
  let delta_v = v1 -| v0 in
  let step_v = delta_v /|. float n in
    v0 +| (step_v *|. float m)

let fold_along_vector f x0 ((v0, v1), n) =
  let open Vector in
  let delta_v = v1 -| v0 in
  let step_v = delta_v /|. float n in
  let at = ref v0 in
  let x = ref x0 in
    for c = 0 to n - 1 do
      x := f !x !at;
      at := !at +| step_v;
    done;
    !x

let get image (x, y) = image (int_of_float (x +. 0.5), int_of_float (y +. 0.5))

let sum xs = Array.fold_left (+.) 0.0 xs 

let sum_list xs = List.fold_left (+.) 0.0 xs 

let minimum xs = Array.fold_left min infinity xs

let maximum xs = Array.fold_left max neg_infinity xs

let average xs = sum xs /. float (Array.length xs) 

let average_list xs = sum_list xs /. float (List.length xs) 

let convolution_1d_cyclic kernel xs = 
  let result = Array.make (Array.length xs) 0.0 in
    for x_at = 0 to Array.length xs - 1 do
      for k_at = 0 to Array.length kernel - 1 do
	let at = wrap_range (Array.length xs) (x_at + k_at - Array.length kernel / 2) in
	  result.(x_at) <- result.(x_at) +. xs.(at) *. kernel.(k_at)
      done
    done;
    result

let box_filter size = (size, Array.make (size * size) (1.0 /. float (size * size)))

let convolution_2d (kernel_size, kernel) (w, h) image =
  assert (kernel_size mod 2 = 1);
  let dst = Array.make (w * h) 0.0 in
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
	let s = ref 0.0 in
	  for xc = -kernel_size / 2 to kernel_size / 2 do
	    for yc = -kernel_size / 2 to kernel_size / 2 do
	      let x' = x + xc in
	      let y' = y + yc in
	      let v = if x' >= 0 && x' < w && y' >= 0 && y < h then image (x', y') else 0.0 in
	      let k = kernel.((xc + kernel_size / 2) + (yc + kernel_size / 2) * kernel_size) in
		s := !s +. v *. k
	    done
	  done;
	  dst.(y * w + x) <- !s
      done
    done;
    fun (x, y) ->
      if x >= 0 && y >= 0 && x < w && y < h
      then dst.(y * w + x)
      else 0.0

let pixels_along_vector image ((_, n) as span) = 
  let pxs = Array.make n 0.0 in
    ignore (
      fold_along_vector
	(fun nth at ->
	   pxs.(nth) <- get image at;
	   nth + 1
	)
	0
	span
    );
    pxs

let edge_vectors image_dims = 
  let (w, h) = image_dims in
  let (w, h) = (float w, float h) in
    [(vector (1.0, 0.0),		vector (w -. 2.0, 0.0));
     (vector (w -. 1.0, 1.0),		vector (w -. 1.0, h -. 2.0));
     (vector (w -. 1.0, h -. 1.0),	vector (1.0, h -. 1.0));
     (vector (0.0, h -. 1.0),		vector (0.0, 1.0))]

let rgb24_of_file filename = 
  match Images.load filename [] with
    | Images.Rgb24 x -> x
    | _ -> failwith "Unsupported bitmap type"

let gray8_of_rgb24 rgb24 =
  let (h, w) = Rgb24.(rgb24.height, rgb24.width) in
  let g = Index8.create w h in
  let buf = String.create w in
    for y = 0 to h - 1 do
      let src = Rgb24.get_scanline rgb24 y in
	for x = 0 to w - 1 do
	  let o = Char.code in
	  let (r, g, b) = (o src.[x * 3], o src.[x * 3 + 1], o src.[x * 3 + 2]) in
	  let g = int_of_float (float r *. 0.2989 +. float g *. 0.5870 +. float b *. 0.1140) in
	    buf.[x] <- Char.chr g
	done;
	Index8.set_scanline g y buf
    done;
    g

let fun_of_gray8 gray8 = 
  let (h, w) = Index8.(gray8.height, gray8.width) in
    fun (x, y) ->
      if x < 0 || x >= w || y < 0 || y >= h
      then 0.0
      else float (Index8.get gray8 x (h - 1 -y)) /. 255.0

let surface_of_rgb24 rgb24 =
  let (w, h) = Rgb24.(rgb24.width, rgb24.height) in
  let module A1 = Bigarray.Array1 in
  let s = Sdlvideo.create_RGB_surface [] ~w ~h ~rmask:0xff0000l ~gmask:0x00ff00l ~bmask:0x0000ffl ~amask:0l ~bpp:24 in
    Sdlvideo.lock s;
    let pixels = Sdlvideo.pixel_data s in
    let pitch = Sdlvideo.((surface_info s).pitch) in
      for y = 0 to h - 1 do
	let src = Rgb24.get_scanline rgb24 y in
	  for x = 0 to w - 1 do
	    let o = Char.code in
	    let (r, g, b) = (o src.[x * 3], o src.[x * 3 + 1], o src.[x * 3 + 2]) in
	    let ofs = (x * 3 + y * pitch) in
	      A1.set pixels (ofs + 2) r;
	      A1.set pixels (ofs + 1) g;
	      A1.set pixels (ofs + 0) b;
	  done;
      done;
      Sdlvideo.unlock s;
      s

let surface_of_fn (w, h) fn =
  let module A1 = Bigarray.Array1 in
  let s = Sdlvideo.create_RGB_surface [] ~w ~h ~rmask:0xff0000l ~gmask:0x00ff00l ~bmask:0x0000ffl ~amask:0l ~bpp:24 in
    Sdlvideo.lock s;
    let pixels = Sdlvideo.pixel_data s in
    let pitch = Sdlvideo.((surface_info s).pitch) in
      for y = 0 to h - 1 do
	for x = 0 to w - 1 do
	  let (r, g, b) = fn (x, y) in
	  let ofs = (x * 3 + y * pitch) in
	    A1.set pixels (ofs + 2) r;
	    A1.set pixels (ofs + 1) g;
	    A1.set pixels (ofs + 0) b;
	done;
      done;
      Sdlvideo.unlock s;
      s

let surface_of_gray_fn (w, h) fn =
  surface_of_fn (w, h) (fun (x, y) -> 
			  let c = int_of_float ((max 0.0 (min 1.0 (fn (x, h - y - 1)))) *. 255.0) in
			    (c, c, c))

let rec wait_exit () =
  match Sdlevent.wait_event () with
    | Sdlevent.QUIT -> ()
    | _ -> wait_exit ()

let rec map_scan_list f xs =
  match xs with
    | [] -> []
    | x::xs -> f x xs::map_scan_list f xs

let map_pairs (f : 'a -> 'a -> 'b) (xxs : 'a list list) : 'b list =
  (List.concat -| List.concat -| List.concat -| List.concat) (
    map_scan_list
      (fun x1s yys ->
	 List.map 
	   (fun x2s ->
	      List.map 
		(fun x1 ->
		   List.map
		     (fun x2 ->
			if x1 != x2 
			then [f x1 x2]
			else []
		     )
		     x2s
		)
		x1s
	   )
	   yys
      )
      xxs
  )

let pairwise (xs : 'a list list) : ('a * 'a) list =
  map_pairs (fun a b -> (a, b)) xs

let line_to_bounds ((x1, y1), (x2, y2)) x =
  let bounds = [((x1, y1), (x1, y2));
		((x1, y2), (x2, y2));
		((x2, y2), (x2, y1));
		((x2, y1), (x1, y1))] in
  let open Vector in
  let intersects = BatList.filter_map (intersect_u_float x) bounds in
  let ahead, behind = List.partition (fun (dem, _) -> dem >= 0.0) (List.map fst intersects) in
  let sort neg = BatList.sort ~cmp:(fun (a, _) (b, _) -> if neg then compare b a else compare a b) in
  let ahead, behind = sort false ahead, sort true behind in
    match behind, ahead with
      | a::_, b::_ -> (Lazy.force (snd a), Lazy.force (snd b))
      | _ -> assert false

let line_of_angle_offset (w, h) (angle, offset) =
  let (w, h) = (float w, float h) in
  let base_x, base_y = cos angle, sin angle in
  let offset_v = Vector.(rot90 (base_x, base_y) *|. offset) in
    line_to_bounds ((0.0, 0.0), (w, h)) 
      Vector.((w /. 2.0, h /. 2.0) +| offset_v,
	      (w /. 2.0 +. base_x, h /. 2.0 +. base_y) +| offset_v)

let angle_offset_of_line (w, h) (a, b) =
  let origo = Vector.((float w, float h) /|. 2.0) in
  let (dx, dy) = Vector.(b -| a) in
  let base = Vector.rot90 (Vector.unit (dx, dy)) in
  let angle = atan2 dy dx in
  let offset = Vector.dot2 base Vector.(a -| origo) in
    (angle, offset)

let avg_of_line image (w, h) (angle, offset) =
  let line = line_of_angle_offset (w, h) (angle, offset) in
  let span = (line, ab_length line) in
  let pxs = pixels_along_vector image span in
  let avg = average pxs in
    avg

let angle_offset_cost image (w, h) (angle, offset) =
  let open BatEnum in
  let get_avg ofs = avg_of_line image (w, h) (angle, offset +. ofs) in
  let rec scan_offset n =
    if n < 10 then
      let ofs = float n *. 2.0 in
      let avg1 = get_avg ofs in
      let avg2 = get_avg (~-. ofs) in
      let value = avg1 *. avg2 in
	if value < 0.2
	then 0.0
	else value +. scan_offset (n + 1)
    else
      0.0
  in
  let avg0 = get_avg 0.0 in
    avg0 *. avg0 +. scan_offset 1

let optimize_angle_offset image dims (angle, offset) =
  let cost x =
    let c = angle_offset_cost image dims x in
      (* Printf.printf "%f %f -> %f\n" (fst x) (snd x) c; *)
      c
  in
  let step step ((angle, offset) as x) = 
    let g = 
      Optimize.gradient2 ~epsilon:(10.0 /. 180.0 *. pi, 4.0)
	(fun x -> cost (x, offset))
	(fun x -> cost (angle, x))
    in
    (* let step = Vector.(g x *| (0.05, 5.0)) in *)
    let step = Vector.(g x *| (0.25, 100.0) *|. 3.0 /|. (float step +. 10.0)) in
      Printf.printf "Stepping %f %f\n" (fst step) (snd step);
      Vector.(x +| step)
  in
    Optimize.optimize ~max_steps:100 ~epsilon:0.00000001 (angle, offset) cost step

let timing label f a0 =
  let t0 = Unix.times () in
  let v = f a0 in
  let t1 = Unix.times () in
    Printf.printf "%s %f\n%!" label Unix.(t1.tms_utime -. t0.tms_utime);
    v

let main () =
  let filename = Sys.argv.(1) in
  let rgb24 = rgb24_of_file filename in
  let image = (fun_of_gray8 -| gray8_of_rgb24) rgb24 in
  let (w, h) as image_dims = Rgb24.(rgb24.width, rgb24.height) in
  let image_smooth = convolution_2d (box_filter 9) image_dims image in
  let edges = edge_vectors image_dims in
  let points =
    flip List.map edges **> fun ab ->
      let span = (ab, ab_length ab) in
      let pxs = pixels_along_vector image span in
      let avg =  average pxs in
      let pxs = convolution_1d_cyclic (Array.make 40 (1.0 /. 40.0)) pxs in
      (* let pxs = Array.map (fun x -> if x >= avg then x else 0.0) pxs in *)
      let maximas = local_maximas ~limit:(fun x -> x >= avg) pxs 60 in
	Printf.printf "%d maximas: %s\n" 
	  (List.length maximas)
	  (String.concat "," (List.map string_of_int maximas));
	List.map (nth_step span) maximas
  in
  let surface = Sdlvideo.set_video_mode ~w:(fst image_dims) ~h:(snd image_dims) ~bpp:24 [] in
  let image_surface = surface_of_gray_fn image_dims image_smooth in
  let points_flat = List.concat points in 
  let point_pairs = pairwise points in
  let map_to_surface (x, y) = (int_of_float x, h - int_of_float y - 1) in
  (* let point_pairs = [List.nth point_pairs 10] in *)
  let point_pairs_good =
    List.filter
      (fun ab ->
	 let pxs = pixels_along_vector image (ab, ab_length ab) in
	 let pxs = convolution_1d_cyclic (Array.make 40 (1.0 /. 40.0)) pxs in
	 let avg = average pxs in
	 let min = minimum pxs in
	 let max = maximum pxs in
	   Printf.printf "%f %f %f\n%!" avg min max;
	   avg > 0.4 && min > 0.3
      )
      point_pairs
  in
  (* let point_pairs_good = *)
  (*   point_pairs_good |> *)
  (* 	List.map *)
  (* 	(fun ab -> *)
  (* 	   let pxs = pixels_along_vector image (ab, ab_length ab) in *)
  (* 	     (average pxs, ab) *)
  (* 	) |> List.sort compare |> List.map snd *)
  (* in *)
  (* let point_pairs_good = BatList.take 4 point_pairs_good in *)
  let angle_offsets = List.map (angle_offset_of_line image_dims) point_pairs_good in
    Sdlvideo.blit_surface ~dst:surface ~src:image_surface ();
    List.iter 
      (fun at ->
	 let (x, y) = map_to_surface at in
	   ignore (Sdlgfx.filledCircleRGBA surface (Sdlvideo.rect x y 0 0) 10 (0xff, 0xff, 0xff) 0xff)
      )
      points_flat;
    Printf.printf "Optimizing\n%!";
    let optimized =
      timing "optimize angles"
	(List.map
	   (fun ao ->
	      let ao' = timing "optimize_angle" (optimize_angle_offset image_smooth image_dims) ao in
		Printf.printf "%f,%f -> %f,%f\n" (fst ao /. pi *. 180.0) (snd ao) (fst ao' /. pi *. 180.0) (snd ao');
		ao'
	   ))
	angle_offsets
    in
      Printf.printf "Done optimizing\n%!";
      List.iter2
	(fun (a, b) ao ->
	   let line (a, b) color =
	     let (x1, y1) = map_to_surface a in
	     let (x2, y2) = map_to_surface b in
	       ignore (Sdlgfx.lineRGBA surface
			 (Sdlvideo.rect x1 y1 0 0)
			 (Sdlvideo.rect x2 y2 0 0)
			 color 0xff)
	   in
	   let (a'o, b'o) = line_of_angle_offset image_dims ao in
	     (* line (a, b) (0xff, 0xff, 0xff); *)
	     line (a'o, b'o) (0x80, 0xff, 0x80);
	)
	point_pairs_good optimized;
      Sdlvideo.update_rect surface;
      wait_exit () 

let _ = main ()
