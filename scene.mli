type face_id

type face = { vs : RecVec.t list; normal : RecVec.t; color : RecVec.t; }

module FaceMap : Map.S with type key = face_id

type face_map = face FaceMap.t

type scene = face_map

val add : face -> scene -> scene

val empty : scene

val map_face_vertices : (RecVec.t -> RecVec.t) -> face -> face

val map_scene_vertices : (RecVec.t -> RecVec.t) -> scene -> scene

val fold_face_vertices : ('a -> RecVec.t -> 'a) -> 'a -> face -> 'a

val fold_scene_faces : ('a -> 'b -> 'a) -> 'a -> 'b FaceMap.t -> 'a

val fold_scene_vertices : ('a -> RecVec.t -> 'a) -> 'a -> scene -> 'a

val num_scene_faces : scene -> int

val num_scene_vertices : scene -> int

val center_scene : scene -> scene

val face0 : RecVec.t -> RecVec.t -> face

val enum_scene_faces : scene -> (FaceMap.key * face) BatEnum.t

val enum_scene_vertices : scene -> RecVec.t Batteries.Enum.t

val scene_of_function :
  (float -> float -> float * RecVec.t) -> float -> int -> int -> scene

val bas_of_scene :
  scene ->
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t *
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t *
  (float, Batteries.Bigarray.float32_elt, Batteries.Bigarray.c_layout)
  Batteries.Bigarray.Array1.t
