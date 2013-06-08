open RecVec

type ('normal, 'color) face = {
  vs	 : t list;
  normal : 'normal;
  color	 : 'color;
}

type ('normal, 'color) scene = ('normal, 'color) face array

let face0 normal color = { vs = []; normal = normal; color = color; }

let ba_of_array3' xs =
  let ps = Bigarray.(Array1.create float32 c_layout (3 * Array.length xs)) in
  Array.iteri (
    fun i { x; y; z } ->
      Bigarray.Array1.(
	set ps (i * 3 + 0) x;
	set ps (i * 3 + 1) y;
	set ps (i * 3 + 2) z
      )
  ) xs;
  ps

let make_grid' f scale width height =
  let size = width * height in 
  let faces = Array.make (size * 2) (face0 vector0 vector0) in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let at x y = 
	let x' = float x /. float width in
	let y' = float y /. float height in
	{ x = scale *. x'; y = scale *. y'; z = fst (f x' y'); }
      in
      let color_at x y = 
	let x' = float x /. float width in
	let y' = float y /. float height in
	snd (f x' y')
      in
      let i = 2 * (x + y * width) in
      let v1 = at (x + 0) (y + 0) in
      let v2 = at (x + 0) (y + 1) in
      let v3 = at (x + 1) (y + 0) in
      faces.(i + 0) <-
	{ vs = [v1; v2; v3];
	  normal = unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1));
	  color = color_at x y; };
      let v1 = at (x + 0) (y + 1) in
      let v2 = at (x + 1) (y + 1) in
      let v3 = at (x + 1) (y + 0) in
      faces.(i + 1) <-
	{ vs = [v1; v2; v3];
	  normal = unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1));
	  color = color_at (x) (y); };
    done
  done;
  faces    

let bas_of_scene scene =
  let vertices =
    Array.init (Array.length scene * 3) (
      fun i ->
	let face = scene.(i / 3) in
	let vertex = List.nth face.vs (i mod 3) in
	vertex
    )
  in
  let normals =
    Array.init (Array.length scene * 3) (
      fun i ->
	let face = scene.(i / 3) in
	face.normal
    )
  in
  let colors =
    Array.init (Array.length scene * 3) (
      fun i ->
	let face = scene.(i / 3) in
	face.color
    )
  in
  (ba_of_array3' vertices, ba_of_array3' normals, ba_of_array3' colors)

let make_rgb_grid f width height =
  let size = width * height in 
  let ar = Bigarray.(Array1.create float32 c_layout (2 * 3 * 3 * size)) in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let i = 2 * 3 * 3 * (x + y * width) in
      let set_at i x y =
	let (r, g, b) = (f (float x /. float (width - 1)) (float y /. float (height - 1))) in
	Bigarray.Array1.(
	  set ar (i + 0) r;
	  set ar (i + 1) g;
	  set ar (i + 2) b;
	)
      in
      set_at (i +  0) (x + 0) (y + 0);
      set_at (i +  3) (x + 0) (y + 1);
      set_at (i +  6) (x + 1) (y + 0);
      set_at (i +  9) (x + 0) (y + 1);
      set_at (i + 12) (x + 1) (y + 1);
      set_at (i + 15) (x + 1) (y + 0);
    done
  done;
  ar    
    
let array_of_bigarray1 ar = Array.init (Bigarray.Array1.dim ar) (fun i -> Bigarray.Array1.get ar i)

let ba1_map f ar = 
  let open Bigarray.Array1 in
  let ar' = create (kind ar) (layout ar) (dim ar) in
  for x = 0 to dim ar - 1 do
    set ar' x (f (get ar x))
  done;
  ar'

let ba1_mapi f ar = 
  let open Bigarray.Array1 in
  let ar' = create (kind ar) (layout ar) (dim ar) in
  for x = 0 to dim ar - 1 do
    set ar' x (f x (get ar x))
  done;
  ar'

let map_face_vertices f face = { face with vs = List.map f face.vs }

let map_scene_vertices f scene = Array.map (map_face_vertices f) scene

let fold_face_vertices f v0 face = List.fold_left f v0 face.vs

let fold_scene_faces f v0 scene = Array.fold_left f v0 scene

let fold_scene_vertices f v0 scene = fold_scene_faces (fun v face -> fold_face_vertices f v face) v0 scene

let center_scene scene =
  let (sum, count) = fold_scene_vertices (fun (sum, count) v -> (sum +.|. v, count + 1)) (vector0, 0) scene in
  let count = float count in
  let center = { x = sum.x /. count; y = sum.y /. count; z = sum.z /. count } in
  map_scene_vertices (fun v -> v -.|. center) scene
