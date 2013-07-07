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

type image = (int * int) -> float

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

let extra_features (x, y) = 
  [|x ** 2.0; y ** 2.0; x ** 3.0; y ** 3.0|]
  (* [||] *)

let features (x, y) =
  Array.concat [[|x; y|]; extra_features (x, y)]

let num_features = 6

let incr_wrap max x =
  incr x;
  if !x >= max then
    x := !x - max

let wrap_range max x =
  let mx = x mod max in
  if mx < 0 
  then max + mx
  else mx

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

let roi_region roi rgb24 =
  match roi with
  | None -> 
    let (h, w) = Rgb24.(rgb24.height, rgb24.width) in
    (0, 0, w, h)
  | Some roi -> roi

let region_wh (x0, y0, x1, y1) = (x1 - x0, y1 - y0)

let region_size region = 
  let (w, h) = region_wh region in
  w * h

let get image (x, y) = image (int_of_float (x +. 0.5), int_of_float (y +. 0.5))

let get_pixels ~roi image =
  let (x0, y0, _, _) = roi in
  let (w, h) = region_wh roi in
  Array.init (w * h) 
    (fun i ->
      let x = x0 + i mod w in
      let y = y0 + i / w in
      image (x, y)
    )

let part ~roi fractile image =
  let xs' = 
    let x = get_pixels ~roi image in
      Array.sort compare x;
      x
  in
  let len = region_size roi in
  let n' = min (len - 1) (int_of_float (float len *. fractile)) in
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

let image_of_array ?roi (w, h) bm =
  let (x0, y0, x1, y1) = match roi with
    | None -> (0, 0, w, h)
    | Some roi -> roi
  in
  fun (x, y) ->
    if x >= x0 && y >= y0 && x < x1 && y < y1
    then bm.((y - y0) * w + (x - x0))
    else 0.0

let convolution_2d ~roi (kernel_size, kernel) image =
  let (x0, y0, x1, y1) as roi = roi in
  let (w, h) = region_wh roi in
  assert (kernel_size mod 2 = 1);
  let dst = Array.make (w * h) 0.0 in
    for x = x0 to x1 - 1 do
      for y = y0 to y1 - 1 do
	let s = ref 0.0 in
	  for xc = -kernel_size / 2 to kernel_size / 2 do
	    for yc = -kernel_size / 2 to kernel_size / 2 do
	      let x' = x + xc in
	      let y' = y + yc in
	      let v = if x' >= x0 && x' < x1 && y' >= y0 && y < y1 then image (x', y') else 0.0 in
	      let k = kernel.((xc + kernel_size / 2) + (yc + kernel_size / 2) * kernel_size) in
		s := !s +. v *. k
	    done
	  done;
	  dst.((y - y0) * w + (x - x0)) <- !s
      done
    done;
    image_of_array ~roi (w, h) dst

let cache_image ~roi image =
  let (x0, y0, _, _) = roi in
  let (w, h) = region_wh roi in
  let copy = Array.init (w * h) (fun c -> image (c mod w + x0, c / w + y0)) in
  image_of_array ~roi (w, h) copy

let map_image f (image : image) =
  fun at ->
    f (image at)

let map_image_xy f (image : image) =
  fun at ->
    f (image at) at
      
let clamp_image ~roi lower higher (image : image) =
  let limit_min = part ~roi lower image in
  let limit_max = part ~roi higher image in
  let do_clamp x = 
    match () with
      | _ when x > limit_max -> x
      | _ when x < limit_min -> 0.0
      | _ -> x
  in
  map_image do_clamp image

module MultiSet = functor(T : Set.OrderedType) -> struct
  type key = T.t
  module M = Map.Make(T)
  type t = int M.t
  let add x t = 
    try 
      let ref_count = M.find x t + 1 in
      M.add x ref_count t
    with Not_found ->
      M.add x 1 t
  let remove x t =
    try 
      let ref_count = M.find x t - 1 in
      if ref_count = 0 then
	M.remove x t
      else
	M.add x ref_count t
    with Not_found ->
      t
  let empty = M.empty
  let min t = fst (M.min_binding t)
  let max t = fst (M.max_binding t)
end

module FloatMultiSet = MultiSet(struct type t = float let compare = compare end)

let adaptive_balance ~roi size image =
  let (x0, y0, x1, y1) = roi in
  let (w, h) = region_wh roi in
  let min_table_horiz = Array.make (w * h) 0.0 in
  let max_table_horiz = Array.make (w * h) 0.0 in
  let min_table_vert = Array.make (w * h) 0.0 in
  let max_table_vert = Array.make (w * h) 0.0 in
  let index x y = (x - x0) + (y - y0) * w in
  for x = x0 to x1 - 1 do
    let values = ref FloatMultiSet.empty in
    for y = y0 to y0 + size / 2 - 1 do
      values := FloatMultiSet.add (image (x, y)) !values
    done;
    for y = y0 to y1 - 1 do
      if y + size / 2 < y1 then
	values := FloatMultiSet.add (image (x, y + size / 2)) !values;
      if y - size / 2 >= y0 then
	values := FloatMultiSet.remove (image (x, y - size / 2)) !values;
      min_table_vert.(index x y) <- FloatMultiSet.min !values;
      max_table_vert.(index x y) <- FloatMultiSet.max !values;
    done
  done;
  for y = y0 to y1 - 1 do
    let values = ref FloatMultiSet.empty in
    for x = x0 to x0 + size / 2 - 1 do
      values := FloatMultiSet.add (image (x, y)) !values
    done;
    for x = x0 to x1 - 1 do
      if x + size / 2 < y1 then
	values := FloatMultiSet.add (image (x + size / 2, y)) !values;
      if x - size / 2 >= x0 then
	values := FloatMultiSet.remove (image (x - size / 2, y)) !values;
      min_table_horiz.(index x y) <- FloatMultiSet.min !values;
      max_table_horiz.(index x y) <- FloatMultiSet.max !values;
    done
  done;
  fun (x, y) ->
    if x >= x0 && y >= y0 && x < x1 && x < y1 then
      let min' = min (min_table_horiz.(index x y)) (min_table_vert.(index x y)) in
      let max' = max (max_table_horiz.(index x y)) (max_table_vert.(index x y)) in
      (image (x, y) -. min') /. (max' -. min')
    else
      0.0

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

let index8_of_rgb24 ?roi f rgb24 =
  let (x0, y0, x1, y1) as roi = roi_region roi rgb24 in
  let (w, h) = region_wh roi in
  let g = Index8.create w h in
  let buf = String.create w in
    for y = y0 to y1 - 1 do
      let src = Rgb24.get_scanline rgb24 y in
	for x = x0 to x1 - 1 do
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

let surface_of_fn ~roi (w, h) fn =
  let module A1 = Bigarray.Array1 in
  let (x0, y0, x1, y1) = roi in
  let s = Sdlvideo.create_RGB_surface [] ~w ~h ~rmask:0xff0000l ~gmask:0x00ff00l ~bmask:0x0000ffl ~amask:0l ~bpp:24 in
    Sdlvideo.lock s;
    let pixels = Sdlvideo.pixel_data s in
    let pitch = Sdlvideo.((surface_info s).pitch) in
      for y = y0 to y1 - 1 do
	for x = x0 to x1 - 1 do
	  let (r, g, b) = fn (x, y) in
	  let ofs = (x * 3 + y * pitch) in
	    A1.set pixels (ofs + 2) r;
	    A1.set pixels (ofs + 1) g;
	    A1.set pixels (ofs + 0) b;
	done;
      done;
      Sdlvideo.unlock s;
      s

let surface_of_gray_fn ~roi (w, h) fn =
  surface_of_fn ~roi (w, h) (
    fun (x, y) -> 
      let c = int_of_float ((max 0.0 (min 1.0 (fn (x, h - y - 1)))) *. 255.0) in
      (c, c, c)
  )

let rec wait_exit () =
  match Sdlevent.wait_event () with
    | Sdlevent.QUIT -> ()
    | _ -> wait_exit ()

let rec map_scan_list f xs =
  match xs with
    | [] -> []
    | x::xs -> f x xs::map_scan_list f xs

(** [find_image_regions condition (width, height) image] finds
    consecutive regions satisfying [condition (image x y)], where x
    ranges 0..width-1 and y 0..height-1. *)
let find_image_regions ~roi condition image =
  let (x0, y0, x1, y1) = roi in
  let (w, h) = region_wh roi in
  let visited = Array.make (w * h) None in
  let index x y = (x - x0) + (y - y0) * w in
  let region_id = ref 0 in
  let regions = Hashtbl.create 10 in
  let rec visit ?id x y =
    if x < x0 || y < y0 || x >= x1 || y >= y1 
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
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
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

let find_laser_dot ?roi surface rgb24 =
  let roi = roi_region roi rgb24 in
  let (w, h) as image_dims = region_wh roi in 
  let image_filtered = 
    let filter () =
      let img = (fun_of_gray8 -| index8_of_rgb24 (filter_color (255, 71, 0) 0.05)) rgb24 in
      let img = clamp_image ~roi 0.999 1.00 img in
      let img = convolution_2d ~roi (gaussian 0.7 0.7 15) img in
      let img = clamp_image ~roi 0.9999 1.00 img in
	img
    in
      timing "filter" filter ()
  in
  let image_surface = surface_of_gray_fn image_dims image_filtered in
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

let find_image_z_offset (surface, kernel) rgb24 =
  let point = find_laser_dot surface rgb24 in
  match point with
  | None -> None
  | Some ((x, y) as _point) ->
    debug "point: (%f, %f)\n%!" x y;
    let z_offset =
      Optimize.linreg_hypo
	(kernel.Optimize.lr_normalize (features (x, y)))
	kernel.Optimize.lr_theta
    in
    Printf.printf "z-offset: %f\n" z_offset;
    Some z_offset

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

let learn_offset (samples : (z_offset * point) list) =
  let samples = Array.of_list samples in
  let training_data = Array.map (fun (z_offset, (x, y)) -> (features (x, y), z_offset)) samples in
    Optimize.linreg
      ~max_steps:50000 ~min_steps:10000 ~epsilon:0.00000000001
      0.001
      (Array.make (num_features + 1) 0.0)
      training_data

let env_of_images display_surface samples =
  let samples_points = 
    List.filter_map (
      fun (rgb24, z_offset) -> 
	let point = find_laser_dot display_surface (Lazy.force rgb24) in
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

let array_sum xs = Array.fold_left ( + ) 0 xs

let array_remove xs i = Array.concat [Array.left xs i; Array.right xs (Array.length xs - i - 1)]

let list_remove xs i = List.take i xs @ List.drop (i + 1) xs 

let long_exposure video =
  let n_frames = 5 in
  let frames = List.init n_frames (fun _ -> (V4l2.get_frame video)#rgb) in
  let frames = List.map int_array_of_string frames in
  let frame_brightness = List.mapi (fun i x -> (array_sum x, i)) frames in
  let frame_brightness = List.sort compare frame_brightness in
  let darkest_idx = snd (List.nth frame_brightness 0) in
  let brightest_idx = snd (List.nth frame_brightness (List.length frame_brightness - 1)) in
  let frames = (flip list_remove darkest_idx % flip list_remove brightest_idx) frames in
  let frame_size = Array.length (List.nth frames 0) in
  let accum = Array.make frame_size 0 in
  let add_ints a b = Array.mapi (fun idx x -> x + b.(idx)) a in
  let accum = List.fold_left ~f:add_ints ~init:accum frames in
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
	let yc' = 
	  if odd
	  then yc
	  else y_steps - yc - 1
	in
	let y =
	  let distance = h /. float (y_steps - 1) *. float yc in
	  if odd
	  then y0 +. distance
	  else y1 -. distance
	in
	  Cnc.wait cnc (Cnc.move [`X x; `Y y]);
	  Cnc.wait cnc Cnc.synchronize;
	  wait_camera video;
	  let frame = timing "long_exposure" long_exposure video in
	    output_file (Printf.sprintf "image-%d-%d.raw" xc yc') frame;
	    results.(yc').(xc) <- Some (apply frame)
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
	  samples := (lazy rgb24, ~-. z_offset)::!samples;
    done;
    Cnc.wait cnc (Cnc.move [`Z max_deviation]);
    Cnc.wait cnc (Cnc.set_position [`Z z]);
    Cnc.wait cnc Cnc.motors_off;
    env_of_images surface !samples

let enable_laser = Cnc.set_port 9 255

let disable_laser = Cnc.set_port 9 0

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
    Cnc.wait cnc (Cnc.set_power true);
    Cnc.wait cnc enable_laser;
    Unix.sleep 1;
    Cnc.wait cnc (Cnc.home [`Z]);
    Cnc.wait cnc (Cnc.move [`Z 20.0]);
    let (x, y, z) = Cnc.wait cnc Cnc.where in
      Printf.printf "X:%f Y:%f Z:%f\n%!" x y z;
      ( try
	  Cnc.wait cnc (Cnc.set_step_speed 5000.0);
	  Cnc.wait cnc (Cnc.set_acceleration [`X 500.0; `Y 500.0]);
	  Cnc.wait cnc (Cnc.move [`X (bed_width /. 2.0); `Y (bed_height /. 2.0)]);
	  let env = auto_calibrate surface cnc video dims 0.5 9 in
	  let table =
	    auto_acquire cnc video
	      (fun frame -> find_image_z_offset env (rgb24_of_string dims frame))
	      ((clearance_x, clearance_y), (bed_width -. clearance_x, bed_height -. clearance_y))
	      (5, 5)
	  in
	    Printf.printf "Calibration table:\n";
	    for y = Array.length table - 1 downto 0 do
	      for x = 0 to Array.length table.(y) - 1 do
		let v = table.(y).(x) in
		  match Option.map classify_float v with
		    | Some (FP_subnormal | FP_zero | FP_normal) -> Printf.printf "% .3f " (Option.get v)
		    | Some (FP_infinite | FP_nan) | None -> Printf.printf "      "
	      done;
	      Printf.printf "\n"
	    done
	with exn ->
	  Printf.printf "exception: %s\nbacktrace: %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ()) );
      Cnc.wait cnc (Cnc.move [`X x; `Y y; `Z z]);
      Cnc.wait cnc disable_laser;
      Cnc.wait cnc Cnc.motors_off;
      Cnc.wait cnc (Cnc.set_power false);
      ()
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
		   ignore (find_image_z_offset env (rgb24_of_file filename))) (List.rev queries)

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
