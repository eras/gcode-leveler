open Batteries
open RecVec

type face_id = int
type vertex_id = int

type vertex = (vertex_id * RecVec.t)

type face_aux = {
  fa_normal   : RecVec.t;
  fa_color    : RecVec.t;
}

type face = {
  face_id  : face_id;
  vs	   : vertex list;
  f_aux    : face_aux;
}

type face_internal = {
  fi_vs	      : vertex_id list;
  fi_aux      : face_aux;
}

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

type triangle = face

let face' vs f_aux = { face_id = mk_face_id (); vs; f_aux = f_aux }
let face vs fa_normal fa_color = face' vs { fa_normal; fa_color }

let triangle (v1, v2, v3) fa_normal fa_color = face [v1; v2; v3] fa_normal fa_color

let recvec_of_vertex : vertex -> RecVec.t = snd

let vertex_of_recvec : RecVec.t -> vertex = fun x -> (mk_vertex_id (), x)

module IdMap =
  Map.Make (
    struct
      type t = face_id
      let compare (x:t) y = compare x y
    end
  )

module InternalFaceMap = IdMap

module VertexMap = IdMap

(* module InternalFaceMap : Map.S with type t = face IdMap.t = IdMap *)

type face_map = face_internal InternalFaceMap.t

type vertex_map = vertex VertexMap.t

type scene = {
  s_faces : face_map;
  s_vertices : vertex_map;
}

let face_aux face = face.f_aux

let fi_of_face face =
  { fi_vs = List.map fst face.vs;
    fi_aux = face.f_aux }

let add face scene =
  { s_faces = InternalFaceMap.add face.face_id (fi_of_face face) scene.s_faces;
    s_vertices = List.fold_left (fun vs ((vertex_id, vector) as vertex) -> VertexMap.add vertex_id vertex vs) scene.s_vertices face.vs; }

let empty : scene = { s_faces = InternalFaceMap.empty; s_vertices = VertexMap.empty; }

let add_vertex vertex scene = { scene with s_vertices = VertexMap.add (fst vertex) vertex scene.s_vertices }

let face_of_face_internal (face_id, face_internal) scene =
  { face_id;
    vs = List.map (fun v -> VertexMap.find v scene.s_vertices) face_internal.fi_vs;
    f_aux = face_internal.fi_aux }

let map_face_vertices f (face_id, face_internal) scene = 
  let face = face_of_face_internal (face_id, face_internal) scene in
  let new_vertices = List.map f face.vs in
  let s_faces = scene.s_faces in
  let s_vertices = scene.s_vertices in
  let s_faces = InternalFaceMap.remove face_id s_faces in
  let s_vertices = List.fold_left (fun s_vertices vertex_id -> VertexMap.remove vertex_id s_vertices) s_vertices face_internal.fi_vs in
  let new_face = face' new_vertices face_internal.fi_aux  in
  add new_face { s_faces; s_vertices }

let map_scene_vertices f scene = 
  InternalFaceMap.fold 
    (fun face_id face_internal scene ->
      map_face_vertices f (face_id, face_internal) scene
    )
    scene.s_faces
    scene

let fold_face_vertices f v0 face scene = List.fold_left (fun accu vertex_id -> f accu (VertexMap.find vertex_id scene.s_vertices )) v0 face.fi_vs

let fold_scene_faces f v0 { s_faces; s_vertices } = InternalFaceMap.fold (fun _ face v -> f v face) s_faces v0

let fold_scene_vertices f v0 scene = fold_scene_faces (fun v face -> fold_face_vertices f v face scene) v0 scene

let num_scene_faces { s_faces } = IdMap.cardinal s_faces

let num_scene_vertices scene = fold_scene_vertices (fun n _ -> (n + 1)) 0 scene

let center_scene scene =
  let (sum, count) = fold_scene_vertices (fun (sum, count) v -> (sum +.|. recvec_of_vertex v, count + 1)) (vector0, 0) scene in
  let count = float count in
  let center = { x = sum.x /. count; y = sum.y /. count; z = sum.z /. count } in
  map_scene_vertices (fun v -> vertex_of_recvec (recvec_of_vertex v -.|. center)) scene

let enum_scene_faces { s_faces } : (InternalFaceMap.key * face_internal) BatEnum.t =
  InternalFaceMap.enum s_faces

let enum_scene_vertices scene =
  Enum.flatten (
    Enum.map
      (fun (_, face_internal) -> List.enum face_internal.fi_vs)
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
	(triangle (vertex_of_recvec v1, vertex_of_recvec v2, vertex_of_recvec v3)
	   (unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1)))
	   (color_at x y))
	!scene;
      let v1 = at (x + 0) (y + 1) in
      let v2 = at (x + 1) (y + 1) in
      let v3 = at (x + 1) (y + 0) in
      scene := add
	(triangle (vertex_of_recvec v1, vertex_of_recvec v2, vertex_of_recvec v3)
	   (unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1)))
	   (color_at x y))
	!scene;
    done
  done;
  !scene

let vertex_of_vertex_id scene vertex_id = VertexMap.find vertex_id scene.s_vertices

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
    let vs = flatten_vertices (Enum.map (vertex_of_vertex_id scene) vs) in
    mk_big vs
  in
  let normals =
    let faces = enum_scene_faces scene in
    mk_big (flatten_recvecs (Enum.map (fun (_, face) -> face.fi_aux.fa_normal) faces))
  in
  let colors =
    let faces = enum_scene_faces scene in
    mk_big (flatten_vertices_thrice (Enum.map (fun (_, face) -> face.fi_aux.fa_color) faces))
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
