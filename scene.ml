open Batteries
open RecVec

type face_id = int
type vertex_id = int

type vertex = (vertex_id * RecVec.t)

type face = {
  vs	 : vertex list;
  normal : RecVec.t;
  color	 : RecVec.t;
}

type triangle = face

let triangle (v1, v2, v3) normal color = { vs = [v1; v2; v3]; normal; color }

let mk_face_id =
  let face_id = ref 0 in
  fun () ->
    incr face_id;
    !face_id

let mk_vertex_id =
  let vertex_id = ref 0 in
  fun () ->
    incr vertex_id;
    !vertex_id

let recvec_of_vertex : vertex -> RecVec.t = snd

let vertex_of_recvec : RecVec.t -> vertex = fun x -> (mk_vertex_id (), x)

module IdMap =
  Map.Make (
    struct
      type t = face_id
      let compare (x:t) y = compare x y
    end
  )

module FaceMap = IdMap

(* module FaceMap : Map.S with type t = face IdMap.t = IdMap *)

type face_map = face IdMap.t

type scene = {
  s_faces : face_map;
}

let add face scene =
  let face_id = mk_face_id () in
  { scene with s_faces = IdMap.add face_id face scene.s_faces }

let empty : scene = { s_faces = FaceMap.empty }

let map_face_vertices f face = { face with vs = List.map f face.vs }

let map_scene_vertices f scene = { scene with s_faces = FaceMap.map (map_face_vertices f) scene.s_faces }

let fold_face_vertices f v0 face = List.fold_left f v0 face.vs

let fold_scene_faces f v0 { s_faces } = FaceMap.fold (fun _ a b -> f b a) s_faces v0

let fold_scene_vertices f v0 scene = fold_scene_faces (fun v face -> fold_face_vertices f v face) v0 scene

let num_scene_faces { s_faces } = IdMap.cardinal s_faces

let num_scene_vertices scene = fold_scene_vertices (fun n _ -> (n + 1)) 0 scene

let center_scene scene =
  let (sum, count) = fold_scene_vertices (fun (sum, count) v -> (sum +.|. recvec_of_vertex v, count + 1)) (vector0, 0) scene in
  let count = float count in
  let center = { x = sum.x /. count; y = sum.y /. count; z = sum.z /. count } in
  map_scene_vertices (fun v -> vertex_of_recvec (recvec_of_vertex v -.|. center)) scene

let enum_scene_faces { s_faces } : (FaceMap.key * face) BatEnum.t =
  FaceMap.enum s_faces

let enum_scene_vertices scene =
  Enum.flatten (
    Enum.map
      (fun (_, face) -> List.enum face.vs)
      (enum_scene_faces scene)
  )

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

let scene_of_function (f : float -> float -> float * RecVec.t) scale width height : scene =
  let scene = ref empty in
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
      let v1 = at (x + 0) (y + 0) in
      let v2 = at (x + 0) (y + 1) in
      let v3 = at (x + 1) (y + 0) in
      scene := add
	{ vs = [vertex_of_recvec v1; vertex_of_recvec v2; vertex_of_recvec v3];
	  normal = unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1));
	  color = color_at x y; }
	!scene;
      let v1 = at (x + 0) (y + 1) in
      let v2 = at (x + 1) (y + 1) in
      let v3 = at (x + 1) (y + 0) in
      scene := add
	{ vs = [vertex_of_recvec v1; vertex_of_recvec v2; vertex_of_recvec v3];
	  normal = unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1));
	  color = color_at (x) (y); }
	!scene;
    done
  done;
  !scene

let bas_of_scene (scene : scene) =
  let flatten_vertices vs =
    Array.concat (List.of_enum (Enum.map (fun (_, {x; y; z}) -> [|x; y; z|]) vs))
  in
  let flatten_recvecs vs =
    Array.concat (List.of_enum (Enum.map (fun {x; y; z} -> [|x; y; z|]) vs))
  in
  let flatten_vertices_thrice vs =
    Array.concat (List.of_enum (Enum.map (fun {x; y; z} -> [|x; y; z; x; y; z; x; y; z|]) vs))
  in
  let mk_big ar = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout ar in
  let vertices =
    let vs = enum_scene_vertices scene in
    let vs = flatten_vertices vs in
    mk_big vs
  in
  let normals =
    let faces = enum_scene_faces scene in
    mk_big (flatten_recvecs (Enum.map (fun (_, face) -> face.normal) faces))
  in
  let colors =
    let faces = enum_scene_faces scene in
    mk_big (flatten_vertices_thrice (Enum.map (fun (_, face) -> face.color) faces))
  in
  (vertices, normals, colors)

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
