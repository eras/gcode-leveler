open Batteries
open Utils

module List =
struct
  include BatList
  include BatList.Labels
end

let ( **> ) a b = a b
let ( -| ) f f2 x = f (f2 x)

module Vector =
struct
  include Vector
  include Vector.Ops
end

type z_offset = float

type angle = float

type offset = float

type angle_offset = (angle * offset)

type vector = (float * float)

let vector ((x : float), (y : float)) = (x, y)

let pi = atan 1.0 *. 4.0

let x_of_vec ((x, y) : vector) = x

let y_of_vec ((x, y) : vector) = y

let ab_length (a, b) = int_of_float (Vector.(length (a -| b)))

let dup a = (a, a)

let project1st f (a, b) = (f a, b)

let project2nd f (a, b) = (a, f b)

let incr_wrap max x =
  incr x;
  if !x >= max then
    x := !x - max

let wrap_range max x =
  let mx = x mod max in
  if mx < 0 
  then max + mx
  else mx

let compress_consecutive condition combine xs =
  let rec loop behind ahead prev =
    match behind, ahead, prev with
      | [], _::_, Some _ -> assert false
      | x::xs, y::ys, Some p when condition p y ->
	  loop (x::xs) ys (Some y)
      | x::xs, y::ys, Some p (* when y > p + 1 *) ->
	  loop (combine x p::xs) (y::ys) None
      | xs, y::ys, None ->
	  loop (y::xs) ys (Some y)
      | xs, [], _ ->
	  xs
  in
    List.rev (loop [] xs None)

let local_maximas ~limit xs ofs0 ofs1 window_size = 
  let module FloatMap = BatMap.Make(BatFloat) in
  let window_contents = Array.make window_size 0.0 in
  let window_map = ref FloatMap.empty in
  let window_add_ofs = ref 0 in
  let window_remove_ofs = ref 0 in
  let window_num_els = ref 0 in
  let remove () =
    let x = window_contents.(!window_remove_ofs) in
      (* debug "Removing %f at %d\n" x !window_remove_ofs; *)
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
    (* debug "Adding %f at %d\n" x !window_add_ofs; *)
    window_contents.(!window_add_ofs) <- x;
    incr_wrap window_size window_add_ofs;
    incr window_num_els;
    try incr (FloatMap.find x !window_map)
    with Not_found -> window_map := FloatMap.add x (ref 1) !window_map
  in 
  let wrap_xs = wrap_range (Array.length xs) in
    for c = 0 to window_size - 1 do
      add xs.(wrap_xs (ofs0 + c - window_size / 2));
    done;
    let maximas = ref [] in
      for x_at = ofs0 to ofs1 do 
	let (range_max, _) = FloatMap.max_binding !window_map in
	  (* debug "%d %f\n" x_at range_max; *)
	  if xs.(x_at) > range_max *. 0.9 && limit xs.(x_at) then
	    maximas := x_at ::!maximas;
	  add xs.(wrap_xs (x_at + window_size / 2));
      done;
      compress_consecutive (fun a b -> b = a + 1) (fun a b -> (a + b) / 2) (List.sort compare !maximas)

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

let part n xs =
  let xs' = 
    let x = Array.copy xs in
      Array.sort compare x;
      x
  in
  let len = Array.length xs in
  let n' = min (len - 1) (int_of_float (float len *. n)) in
    xs'.(n')

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

let gaussian sigma_x sigma_y size =
  let sigma_x22 = 2.0 *. sigma_x ** 2.0 in
  let sigma_y22 = 2.0 *. sigma_y ** 2.0 in
  let f x y = exp (~-.(x ** 2.0 /. sigma_x22 +. y ** 2.0 /. sigma_y22)) in
  let k = 
    Array.init (size * size) 
      (fun c -> 
	 let scale x = (float x /. float (size - 1) -. 0.5) *. 2.0 in
	   f (scale (c mod size)) (scale (c / size))
      )
  in
  let scale = 1.0 /. sum k in
    (size, BatArray.map ( ( *. ) scale) k)

let string_of_kernel (size, m) =
  let b = Buffer.create 1024 in
    for y = 0 to size - 1 do
      for x = 0 to size - 1 do
	if x <> 0 then
	  Buffer.add_char b ' ';
	Printf.ksprintf (Buffer.add_string b) "% 0.3f" m.(x + y * size)
      done;
      Buffer.add_char b '\n'
    done;
    Buffer.contents b

let image_of_array (w, h) bm =
  fun (x, y) ->
    if x >= 0 && y >= 0 && x < w && y < h
    then bm.(y * w + x)
    else 0.0

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
    image_of_array (w, h) dst

let array_of_image (w, h) image = 
  Array.init (w * h) (fun c -> image (c mod w, c / w))

let clamp_image lower higher (w, h) image =
  let src = array_of_image (w, h) image in
  let limit_min = part lower src in
  let limit_max = part higher src in
  let do_clamp x = 
    match () with
      | _ when x > limit_max -> x
      | _ when x < limit_min -> 0.0
      | _ -> x
  in
  let dst = Array.init (w * h) (fun c -> do_clamp (image (c mod w, c / w))) in
    image_of_array (w, h) dst

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
    [
      (vector (1.0, 0.0),		vector (w -. 2.0, 0.0));
      (vector (w -. 1.0, 1.0),		vector (w -. 1.0, h -. 2.0));
      (vector (w -. 1.0, h -. 1.0),	vector (1.0, h -. 1.0));
      (vector (0.0, h -. 1.0),		vector (0.0, 1.0))
    ]

let rgb24_of_file filename = 
  match Images.load filename [] with
    | Images.Rgb24 x -> x
    | _ -> failwith "Unsupported bitmap type"

let rgb24_of_string (width, height) string = 
  let bytes_per_line = width * 3 in
    Rgb24.create_with_scanlines
      width height
      Info.([Info_Depth 24; Info_ColorModel RGB])
      (Array.init height (fun y -> String.sub string (y * bytes_per_line) bytes_per_line))

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
  let intersects = List.filter_map (intersect_u_float x) bounds in
  let ahead, behind = List.partition (fun (dem, _) -> dem >= 0.0) (List.map fst intersects) in
  let sort neg = List.stable_sort ~cmp:(fun (a, _) (b, _) -> if neg then compare b a else compare a b) in
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

(* clamp high values *)
let bounded_avg_of_line image (w, h) (angle, offset) =
  let line = line_of_angle_offset (w, h) (angle, offset) in
  let span = (line, ab_length line) in
  let pxs = pixels_along_vector image span in
  let avg = average pxs in
  let threshold = max (part 0.5 pxs) (avg *. 1.1) in
  (* let threshold = avg *. 1.1 in *)
  let pxs' = BatArray.map (fun x -> if x >= threshold then threshold else x) pxs in
  let avg' = average pxs' in
    avg'

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

let optimize_angle_offset report image dims (angle, offset) =
  let cost x =
    let c = angle_offset_cost image dims x in
      (* debug "%f %f -> %f\n" (fst x) (snd x) c; *)
      c
  in
  let step_angle (angle, offset) =
    let g = 
      Optimize.derivate ~epsilon:(1.0 /. 180.0 *. pi)
	(fun x -> cost (x, offset))
    in
    let step = clamp (-0.005) 0.005 (g angle *. 0.05) in
      debug "Stepping angle %f\n" step;
      angle +. step
  in
  let step_offset (angle, offset) =
    let g = 
      Optimize.derivate ~epsilon:1.0
	(fun x -> cost (angle, x))
    in
    let step = clamp (-1.0) 1.0 (g offset *. 100.0) in
      debug "Stepping offset %f\n" step;
      offset +. step
  in
  let step step (angle, offset) = 
    debug "step %d\n" step;
    let angle = step_angle (angle, offset) in
    let offset = step_offset (angle, offset) in
      report step (angle, offset);
      (angle, offset)
  in
  let x = Optimize.optimize ~max_steps:200 ~epsilon:0.0001 (angle, offset) cost step in
    debug "Done optimizing\n";
    x

let string_of_span (((x1, y1), (x2, y2)), len) =
  Printf.sprintf "(((%f, %f), (%f, %f)), %d)" x1 y1 x2 y2 len

let edge_points image_dims image =
  let edges = edge_vectors image_dims in
  let edge_pxs = List.map (fun ab -> pixels_along_vector image (ab, ab_length ab)) edges in
  let last = List.nth edge_pxs (List.length edge_pxs - 1) in
  let first = List.nth edge_pxs 0 in
  let spans = 
    List.rev (
      snd (
	BatList.fold_left
	  (fun (offset, offsets) (ab, pxs) ->
	    let span = (ab, ab_length ab) in
	    (offset + Array.length pxs, (((offset, offset + Array.length pxs - 1), span)::offsets))
	  )
	  (Array.length last, [])
	  (List.combine edges edge_pxs)
      )
    )
  in
  let edge_pxs_single = Array.concat edge_pxs in
  let edge_loop_pxs = Array.concat [last; edge_pxs_single; first] in
  let avg = average edge_pxs_single in
  let pxs = convolution_1d_cyclic (Array.make 70 (1.0 /. 70.0)) edge_loop_pxs in
  let ofs0 = Array.length first in
  let ofs1 = Array.length edge_loop_pxs - Array.length last - 1 in
  let maximas = local_maximas ~limit:(fun x -> x >= avg) pxs ofs0 ofs1 10 in
    debug "%d maximas: %s\n" 
      (List.length maximas)
      (String.concat "," (List.map string_of_int maximas));
    List.map (
      fun ((ofs0, ofs1), span) ->
	let ms =
	  List.filter
	    (fun x -> x >= ofs0 && x <= ofs1)
	    maximas
	in
	  debug "New edge %d-%d %s\n" ofs0 ofs1 (string_of_span span);
	  List.map (fun ms -> 
		      debug "ms %d -> %d\n" ms (ms - ofs0);
		      nth_step span (ms - ofs0)
		   ) ms
    ) 
      spans

let compress_consecutive_lines aos =
  compress_consecutive
    (fun (a'angle, a'offset) (b'angle, b'offset) ->
       abs_float (a'angle -. b'angle) < 0.01 &&
	 abs_float (a'offset -. b'offset) < 0.5
    )
    (fun (a'angle, a'offset) (b'angle, b'offset) ->
       ((a'angle +. b'angle) /. 2.0,
	(a'offset +. b'offset) /. 2.0))
    (List.sort compare aos)

let analyze surface rgb24 =
  let image = (fun_of_gray8 -| gray8_of_rgb24) rgb24 in
  let (w, h) as image_dims = Rgb24.(rgb24.width, rgb24.height) in
  let image'low = part 0.5 (array_of_image image_dims image) in
  let image' = 
    let filter () =
      let img = image in
      let img = clamp_image 0.5 0.95 image_dims img in
      let img = convolution_2d (gaussian 0.7 0.7 15) image_dims img in
	img
    in
      timing "filter" filter ()
  in
  let points = edge_points image_dims image in
  let image_surface = surface_of_gray_fn image_dims image' in
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
	 let threshold =  0.9 *. average [|pxs.(0); pxs.(Array.length pxs - 1)|] in
	   debug "%f %f %f\n%!" threshold min max;
	   (* avg > 0.4 && min > 0.3 *)
	   avg > threshold
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
  (* let point_pairs_good = List.take 4 point_pairs_good in *)
  let angle_offsets = List.map (angle_offset_of_line image_dims) point_pairs_good in
    List.iter 
      (fun at ->
	 let (x, y) = map_to_surface at in
	   ignore (Sdlgfx.filledCircleRGBA image_surface (Sdlvideo.rect x y 0 0) 10 (0xff, 0xff, 0xff) 0xff)
      )
      points_flat;
    Sdlvideo.blit_surface ~dst:surface ~src:image_surface ();
    Sdlvideo.update_rect surface;
    Printf.printf "Optimizing\n%!";
    let report erase color step (angle, offset) = 
      !erase ();
      let (a, b) = line_of_angle_offset (w, h) (angle, offset) in
      let (x1, y1) = map_to_surface a in
      let (x2, y2) = map_to_surface b in
	ignore (Sdlgfx.lineRGBA surface
		  (Sdlvideo.rect x1 y1 0 0)
		  (Sdlvideo.rect x2 y2 0 0)
		  color 0xff);
	erase := (fun () -> 
		    ignore (Sdlgfx.lineRGBA surface
			      (Sdlvideo.rect x1 y1 0 0)
			      (Sdlvideo.rect x2 y2 0 0)
			      (0x00, 0x00, 0x00) 0xff));
	Sdlvideo.update_rect surface
    in
    let _no_report _ _ = () in
    let optimized =
      timing "optimize angles"
	(BatList.filter_map
	   (fun ao ->
	      let erase = ref (fun () -> ()) in
	      let r = report erase (0x80, 0xff, 0x80) in
	      (* let r = no_report in *)
	      let ao' = timing "optimize_angle" (optimize_angle_offset r image' image_dims) ao in
		debug "%f,%f -> %f,%f\n" (fst ao /. pi *. 180.0) (snd ao) (fst ao' /. pi *. 180.0) (snd ao');
		Sdlvideo.blit_surface ~dst:surface ~src:image_surface ();
		let ab = line_of_angle_offset (w, h) ao' in
		let pxs = pixels_along_vector image (ab, ab_length ab) in
		  if part 0.10 pxs < image'low 
		  then None
		  else (
		    report erase (0x80, 0xff, 0x80) 0 ao';
		    Some ao'
		  )
	   ))
	angle_offsets
    in
    let optimized = compress_consecutive_lines optimized in
      Printf.printf "Done optimizing\n%!";
      Sdlvideo.blit_surface ~dst:surface ~src:(surface_of_rgb24 rgb24) ();
      List.iter
	(fun ao ->
	   let line (a, b) color =
	     let (x1, y1) = map_to_surface a in
	     let (x2, y2) = map_to_surface b in
	       ignore (Sdlgfx.lineRGBA surface
			 (Sdlvideo.rect x1 y1 0 0)
			 (Sdlvideo.rect x2 y2 0 0)
			 color 0xff)
	   in
	   let (a'o, b'o) = line_of_angle_offset image_dims ao in
	     Printf.printf "%f,%f\n" (fst ao /. pi *. 180.0) (snd ao);
	     (* line (a, b) (0xff, 0xff, 0xff); *)
	     line (a'o, b'o) (0x80, 0xff, 0x80);
	)
	optimized;
      Sdlvideo.update_rect surface;
      optimized

let parse_sample_ofs str =
  try
    match Pcre.extract ~full_match:false ~pat:"(.+)=([-0-9.]+)" str with
      | [|name; offset|] -> (name, float_of_string offset)
      | _ -> assert false
  with Not_found -> failwith "Invalid sample name. Must be of format filename=offset"

let usage () =
  Printf.printf "usage: scan pic1.jpg=0.1 pic2.jpg=0.2\n%!"

(* picked from http://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists :( *)
let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let similar_angle a b = abs_float (a -. b) < (10.0 /. 180.0 *. pi)

let offsets_of_angle data base_angle =
  let similar_angle_offset base_angle (angle, _offset) = similar_angle base_angle angle in
    List.filter_map
      (fun (z_offset, aos) ->
	 match List.Exceptionless.find (similar_angle_offset base_angle) aos with
	   | None -> None
	   | Some (_, offset) -> Some (z_offset, offset))
      data

let analyze_data (data : (z_offset * angle_offset list) list) =
  let angles =
    compress_consecutive
      similar_angle
      (fun a b -> (a +. b) /. 2.0)
      (BatList.sort compare **>
	 List.map fst (List.concat (List.map snd data)))
  in
  let angle_zofsofs = List.map (fun angle -> (angle, offsets_of_angle data angle)) angles in
    Printf.printf "training data\n";
    List.iter
      (fun (angle, offsets) ->
	 Printf.printf "angle: %f\n" (angle /. pi *. 180.0);
	 List.iter (fun (z_offset, offset) -> Printf.printf "  z-offset: %f offset: %f\n" z_offset offset) offsets
      )
      angle_zofsofs;
    angle_zofsofs

(* angles: -2.81137104215 0.334435358196 2.18057844162 2.80557972733 3.05623971742 *)
(* aos: (-2.807649,118.332092) (0.334435,-118.207293) (2.176905,28.918486) (2.807629,48.072728) (3.059995,82.298368) *)
(* offsets: 118.332091728 -118.207292594 28.9184862267 48.072727735 82.2983682201 *)

let deg_of_rad rad = rad /. pi *. 180.0

let query (surface, kernels) rgb24 =
  let aos = analyze surface rgb24 in
  let _ = debug "aos: %s\n%!" (String.concat " " (List.map (fun (angle, offset) -> Printf.sprintf "(%f,%f)" angle offset) aos)) in
  let z_offsets =
    List.filter_map
      (fun (angle, kernel) ->
	 Printf.printf "angle: %f\n%!" (deg_of_rad angle);
	 let offsets = Array.of_list (List.filter_map (fun (angle', offset) ->
							    if similar_angle angle angle'
							    then Some offset
							    else None) aos) in
	 let _ = Printf.printf "offsets: %s\n%!" (String.concat " " (List.map string_of_float (Array.to_list offsets))) in
	   if Array.length offsets > 0
	   then Some (Optimize.linreg_hypo
			(kernel.Optimize.lr_normalize [|Array.fold_left min offsets.(0) offsets|])
			kernel.Optimize.lr_theta)
	   else None
      )
      kernels
  in
  let z_offset = Utils.average_list z_offsets in
    Printf.printf "z-offsets: %s\n" (String.concat " " (List.map string_of_float z_offsets));
    Printf.printf "z-offset: %f\n" z_offset;
    z_offset

let map_features xs fs =
  let mapping row =
    let num_els = Array.length row in
      Array.init (num_els * Array.length fs) (fun i -> fs.(i / num_els) row.(i mod num_els))
  in
    (Array.map (fun (data, result) -> (mapping data, result)) xs,
     mapping)

let wait_camera video =
  for c = 1 to 8 do ignore (V4l2.get_frame video); done

let learn_angle (offsets : (z_offset * offset) list) =
  let num_features = 1 in
  let training_data = Array.map (fun (z_offset, offset) -> ([|offset|], z_offset)) (Array.of_list offsets) in
    Optimize.linreg
      ~max_steps:50000 ~min_steps:10000 ~epsilon:0.00000000001
      0.001
      (Array.make (num_features + 1) 0.0)
      training_data

let env_of_images surface samples =
  let angle_offsets = analyze_data (List.map (fun (data, offset) -> (offset, analyze surface (Lazy.force data))) samples) in
    (* let extra_features = [|(fun x -> x ** 2.0); (fun x -> x ** 3.0)|] in *)
  let kernels = List.map (project2nd learn_angle) angle_offsets in
  let env = (surface, kernels) in
    List.iter
      (fun (angle, { Optimize.lr_theta = theta }) ->
	 Printf.printf "angle %f theta:" (angle /. pi *. 180.0);
	 Array.iter (Printf.printf " %f") theta;
	 Printf.printf "\n";
      ) kernels;
    env

let auto_acquire cnc video apply ((x0, y0), (x1, y1)) (x_steps, y_steps) =
  let w = x1 -. x0 in
  let h = y1 -. y0 in
  let results = Array.make_matrix x_steps y_steps None in
    for xc = 0 to x_steps - 1 do
      for yc = 0 to y_steps - 1 do
	let odd = xc mod 2 = 0 in
	let x = x0 +. (w /. float (x_steps - 1) *. float xc) in
	let y =
	  let distance = h /. float (y_steps - 1) *. float yc in
	  if odd
	  then y0 +. distance
	  else y1 -. distance
	in
	  Cnc.wait cnc (Cnc.move [`X x; `Y y]);
	  Cnc.wait cnc Cnc.synchronize;
	  wait_camera video;
	  let frame = (V4l2.get_frame video)#decode in
	    output_file (Printf.sprintf "image-%d-%d.raw" xc yc) frame;
	    results.(yc).(xc) <- Some (apply frame)
      done
    done;
    Array.init y_steps
      (fun y ->
	 Array.init x_steps (fun x -> BatOption.get results.(y).(x))
      )

let auto_calibrate surface cnc video dims max_deviation steps =
  let (_, _, z) = Cnc.wait cnc Cnc.where in
  let samples = ref [] in
    Cnc.wait cnc (Cnc.set_position [`Z max_deviation]);
    for step = 0 to steps - 1 do
      let z_offset = float step *. 2.0 *. max_deviation /. float (steps - 1) -. max_deviation in
	Printf.printf "step %d/%d z offset %f\n%!" (step + 1) steps z_offset;
	Cnc.wait cnc (Cnc.move [`Z (z_offset +. max_deviation)]);
	Cnc.wait cnc Cnc.synchronize;
	wait_camera video;
	let frame = (V4l2.get_frame video)#decode in
	let rgb24 = (rgb24_of_string dims frame) in
	  Sdlvideo.blit_surface ~dst:surface ~src:(surface_of_rgb24 rgb24) ();
	  Sdlvideo.update_rect surface;
	  output_file (Printf.sprintf "%+.2f.raw" z_offset) frame;
	  samples := (lazy rgb24, z_offset)::!samples;
    done;
    Cnc.wait cnc (Cnc.move [`Z max_deviation]);
    Cnc.wait cnc (Cnc.set_position [`Z z]);
    Cnc.wait cnc Cnc.motors_off;
    env_of_images surface !samples

let scan _ =
  (* let clearance_x = 40.0 in *)
  (* let clearance_y = 30.0 in *)
  let clearance_x = 0.0 in
  let clearance_y = 0.0 in
  let dims = (640, 480) in
  let (bed_width, bed_height) = (120.0 -. 40.0, 180.0 -. 30.0) in
  let cnc = Cnc.connect "/dev/ttyACM0" 115200 in
  let video = V4l2.init "/dev/video0" { V4l2.width = fst dims; height = snd dims } in
  let surface = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:24 [] in
    Sys.catch_break true;
    Unix.sleep 1;
    (* Cnc.ignore cnc (Cnc.home [`X;`Y]); *)
    (* Cnc.ignore cnc (Cnc.move [`X (bed_width /. 2.0); `Y (bed_height /. 2.0)]); *)
    Cnc.wait cnc Cnc.motors_off;
    let (x, y, z) = Cnc.wait cnc Cnc.where in
      Printf.printf "X:%f Y:%f Z:%f\n%!" x y z;
      ( try
	  Cnc.wait cnc (Cnc.set_step_speed 5000.0);
	  Cnc.wait cnc (Cnc.set_acceleration [`X 50.0; `Y 50.0]);
	  Cnc.wait cnc (Cnc.move [`X (bed_width /. 2.0); `Y (bed_height /. 2.0)]);
	  let env = auto_calibrate surface cnc video dims 0.5 9 in
	  let table =
	    auto_acquire cnc video
	      (fun frame -> query env (rgb24_of_string dims frame))
	      ((clearance_x, clearance_y), (bed_width -. clearance_x, bed_height -. clearance_y))
	      (5, 5)
	  in
	    Printf.printf "Calibration table:\n";
	    for y = Array.length table - 1 downto 0 do
	      for x = 0 to Array.length table.(y) - 1 do
		let v = table.(y).(x) in
		  match classify_float v with
		    | FP_subnormal | FP_zero | FP_normal -> Printf.printf "% .3f " v
		    | FP_infinite | FP_nan -> Printf.printf "      "
	      done;
	      Printf.printf "\n"
	    done
	with exn ->
	  Printf.printf "exception: %s\nbacktrace: %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ()) );
      Cnc.wait cnc (Cnc.move [`X x; `Y y; `Z z]);
      Cnc.wait cnc Cnc.motors_off
	(* Cnc.move cnc [`X 10.0; `Y 10.0]; *)
	(* Unix.sleep 1 *)
  
let analysis (queries, samples) =
  if List.length samples = 0 
  then usage ()
  else
    let surface = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:24 [] in
    let env = env_of_images surface (List.map (fun (filename, offset) -> (lazy (rgb24_of_file filename), offset)) samples) in
      List.iter (fun filename ->
		   Printf.printf "Quering offset from %s\n%!" filename;
		   ignore (query env (rgb24_of_file filename))) (List.rev queries)

let main () =
  let samples = ref [] in
  let queries = ref [] in
  let task = ref (fun _ -> ()) in
  let args = [("-sample", Arg.String (fun s -> task := analysis; samples := parse_sample_ofs s :: !samples), "Add a new sample in form filename=offset");
	      ("-query", Arg.String (fun s -> queries := s :: !queries), "Query the height of an image");
	      ("-scan", Arg.Unit (fun () -> task := scan), "Set the automatic scan mode");
	     ] in
    Arg.parse args (fun _ -> failwith "unknown argument") "scan - determine z offset from images";
    !task (!queries, !samples)

let _ = main ()
