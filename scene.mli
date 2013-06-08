type face_id
type vertex_id

type vertex

val vertex_of_recvec : RecVec.t -> vertex

type face

val face : vertex list -> RecVec.t -> RecVec.t -> face

type scene

val add : face -> scene -> scene

val empty : scene

val map_face_vertices : (vertex -> vertex) -> face -> face

val map_scene_vertices : (vertex -> vertex) -> scene -> scene

val fold_face_vertices : ('a -> vertex -> 'a) -> 'a -> face -> 'a

val fold_scene_faces : ('a -> face -> 'a) -> 'a -> scene -> 'a

val fold_scene_vertices : ('a -> vertex -> 'a) -> 'a -> scene -> 'a

val num_scene_faces : scene -> int

val num_scene_vertices : scene -> int

val center_scene : scene -> scene

val enum_scene_faces : scene -> (face_id * face) BatEnum.t

val enum_scene_vertices : scene -> vertex Batteries.Enum.t

val scene_of_function : (float -> float -> float * RecVec.t) -> float -> int -> int -> scene

val bas_of_scene :
  scene ->
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t *
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t *
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t
