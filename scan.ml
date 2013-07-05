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

type point = (float * float)

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

let index8_of_rgb24 f rgb24 =
  let (h, w) = Rgb24.(rgb24.height, rgb24.width) in
  let g = Index8.create w h in
  let buf = String.create w in
    for y = 0 to h - 1 do
      let src = Rgb24.get_scanline rgb24 y in
	for x = 0 to w - 1 do
	  let o = Char.code in
	  let rgb = (o src.[x * 3], o src.[x * 3 + 1], o src.[x * 3 + 2]) in
	  let g = f rgb in
	    buf.[x] <- Char.chr g
	done;
	Index8.set_scanline g y buf
    done;
    g

let gray8_of_rgb24 (r, g, b) = int_of_float (float r *. 0.2989 +. float g *. 0.5870 +. float b *. 0.1140) 

let min3 a b c = min (min a b) c

let max3 a b c = max (max a b) c

let mod_float_positive f x =
  let res = mod_float f x in
  let res = if res < 0.0 then x +. res else res in
  res

let h_of_rgb (r, g, b) =
  let m' = max3 r g b in
  let m = min3 r g b in
  let c = m' -. m in
  let h' =
    if m' = m then None
    else if m' = r then Some (mod_float_positive ((g -. b) /. c) 6.0) 
    else if m' = g then Some ((b -. r) /. c +. 2.0)
    else if m' = b then Some ((r -. g) /. c +. 4.0)
    else None
  in
  h'

(* assumes a, b < mod' *)
let mod_distance mod' a b =
  let (a, b) = (min a b, max a b) in
  let d1 = b -. a in
  let d2 = a +. (mod' -. b) in
  min d1 d2

let rgb_hue_distance a =
  let h1 = h_of_rgb a in
  fun b -> 
    let h2 = h_of_rgb b in
    match h1, h2 with
    | None, None -> 0.0
    | Some h1, Some h2 -> mod_distance 6.0 h1 h2 /. 6.0
    | _ -> 1.0

let fun_of_gray8 gray8 = 
  let (h, w) = Index8.(gray8.height, gray8.width) in
    fun (x, y) ->
      if x < 0 || x >= w || y < 0 || y >= h
      then 0.0
      else float (Index8.get gray8 x (h - 1 -y)) /. 255.0

let f3_of_i3 (r, g, b) = let f x = float_of_int x /. 255.0 in (f r, f g, f b)

let filter_color base_color =
  let distance = rgb_hue_distance (f3_of_i3 base_color) in
  fun threshold color ->
    if distance (f3_of_i3 color) < threshold then gray8_of_rgb24 color
    else 0

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


(** [find_image_regions condition (width, height) image] finds
    consecutive regions satisfying [condition (image x y)], where x
    ranges 0..width-1 and y 0..height-1. *)
let find_image_regions condition (w, h) image =
  let visited = Array.make (w * h) None in
  let index x y = x + y * w in
  let region_id = ref 0 in
  let regions = Hashtbl.create 10 in
  let rec visit ?id x y =
    if x < 0 || y < 0 || x >= w || y >= h 
      || visited.(index x y) <> None
      || not (condition (image (x, y)))
    then ()
    else begin
      let id = 
	match id with
	| None -> 
	  let id = !region_id in
	  incr region_id;
	  id
	| Some id -> id
      in
      let list = 
	try Hashtbl.find regions id
	with Not_found -> 
	  let l = ref [] in
	  Hashtbl.add regions id l;
	  l
      in
      list := (x, y)::!list;
      visited.(index x y) <- Some id;
      visit ~id (x - 1) y;
      visit ~id (x + 1) y;
      visit ~id x (y - 1);
      visit ~id x (y + 1);
    end
  in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      visit x y;
    done
  done;
  Hashtbl.fold
    (fun _region points regions ->
      let points = !points in
      let n_points = List.length points in
      let (accum_x, accum_y) =
	List.fold_left
	  ~f:(fun (x1, y1) (x2, y2) -> (x1 + x2, y1 + y2))
	  ~init:(0, 0)
	  points in
      let center = (float accum_x /. float n_points,
		    float accum_y /. float n_points) in
      (sqrt (float n_points) /. pi, center)::regions
    )
    regions
    []

let analyze surface rgb24 =
  let (w, h) as image_dims = Rgb24.(rgb24.width, rgb24.height) in
  let image_gray = (fun_of_gray8 -| index8_of_rgb24 gray8_of_rgb24) rgb24 in
  let image'low = part 0.5 (array_of_image image_dims image_gray) in
  let image_filtered = 
    let filter () =
      let img = (fun_of_gray8 -| index8_of_rgb24 (filter_color (255, 71, 0) 0.05)) rgb24 in
      let img = clamp_image 0.999 1.00 image_dims img in
      let img = convolution_2d (gaussian 0.7 0.7 15) image_dims img in
      let img = clamp_image 0.9999 1.00 image_dims img in
	img
    in
      timing "filter" filter ()
  in
  let image_surface = surface_of_gray_fn image_dims image_filtered in
  let map_to_surface (x, y) = (int_of_float x, h - int_of_float y - 1) in
  (* Sdlvideo.blit_surface ~dst:surface ~src:(surface_of_rgb24 rgb24) (); *)
  Sdlvideo.blit_surface ~dst:surface ~src:image_surface ();
  Sdlvideo.update_rect surface;
  let regions = find_image_regions (fun x -> x > 0.1) image_dims image_filtered in
  let regions = List.fast_sort ~cmp:(fun (size1, _) (size2, _) -> compare size1 size2) regions in
  Printf.printf "%d regions\n%!" (List.length regions);
  match regions with
  | [] -> None
  | (_, center)::_ -> Some center

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
      (fun (z_offset, point) ->
	 match List.Exceptionless.find (similar_angle_offset base_angle) point with
	   | None -> None
	   | Some (_, offset) -> Some (z_offset, offset))
      data

let deg_of_rad rad = rad /. pi *. 180.0

let query (surface, kernel) rgb24 =
  let point = analyze surface rgb24 in
  match point with
  | None -> failwith "Cannot find point, cannot query"
  | Some ((x, y) as point) ->
    debug "point: (%f, %f)\n%!" x y;
    let z_offset =
      Optimize.linreg_hypo
	(kernel.Optimize.lr_normalize [|x; y|])
	kernel.Optimize.lr_theta
    in
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

let learn_offset ?(extra_features=[||]) (samples : (z_offset * point) list) =
  let num_features = 2 in
  let samples = Array.of_list samples in
  let training_data = Array.map (fun (z_offset, (x, y)) -> ([|x; y|], z_offset)) samples in
    Optimize.linreg
      ~max_steps:50000 ~min_steps:10000 ~epsilon:0.00000000001
      0.001
      (Array.make (num_features + 1) 0.0)
      training_data

let env_of_images display_surface samples =
  (* let extra_features = [|(fun x -> x ** 2.0); (fun x -> x ** 3.0)|] in *)
  let samples_points = 
    List.filter_map (
      fun (rgb24, z_offset) -> 
	let point = analyze display_surface (Lazy.force rgb24) in
	match point with
	| None -> None
	| Some point -> Some ((z_offset : z_offset), point)
    ) samples in
  let kernel = learn_offset samples_points in
  let env = (display_surface, kernel) in
  let { Optimize.lr_theta = theta } = kernel in
  Printf.printf "theta: ";
  Array.iter (Printf.printf " %f") theta;
  Printf.printf "\n";
  env

let int_array_of_string str =
  Array.init (String.length str) (fun c -> int_of_char str.[c])

let long_exposure video =
  let n_frames = 10 in
  let frames = Array.init n_frames (fun _ -> (V4l2.get_frame video)#rgb) in
  let frame_size = String.length frames.(0) in
  let accum = Array.make frame_size 0 in
  let add_ints a b = Array.mapi (fun idx x -> x + b.(idx)) a in
  let accum = Array.fold_left add_ints accum (Array.map int_array_of_string frames) in
  let final = Array.map (fun x -> char_of_int (x / n_frames)) accum in
  String.implode (Array.to_list final)

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
	  let frame = long_exposure video in
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
	let frame = long_exposure video in
	Printf.printf "Received %d bytes for a frame\n%!" (String.length frame);
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
