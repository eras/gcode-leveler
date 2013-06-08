open Graph

module FloatWithId = struct
  open Graph.Delaunay.FloatPoints

  type point = (int * ((float * float) * float))
      
  let ccw (_, (p1, _)) (_, (p2, _)) (_, (p3, _)) = ccw p1 p2 p3
  let in_circle (_, (p1, _)) (_, (p2, _)) (_, (p3, _)) (_, (p4, _)) = in_circle p1 p2 p3 p4 
end

module T = Graph.Delaunay.Make(FloatWithId)

let vertices = [|((20.791693,1037.017), 0.0);
		 ((27.195517,409.9173), 10.0);
		 ((197.4631,530.52356), 20.0);
		 ((184.06242,743.35803), 40.0);
		 ((222.68794,899.43671), 50.0);
		 ((383.49622,714.19183), 40.0);
		 ((557.7052,510.81668), 30.0);
		 ((549.82239,854.50494), 20.0);
		 ((700.38312,1000.3359), 10.0);
		 ((691.71204,387.8457), 00.0)|]

let gen_id () =
  let id = ref 0 in
  fun () ->
    let id' = !id in
    incr id;
    id'

let add_id gen_id x = (gen_id (), x)

let faces_of_triangulation t =
  let faces = ref [] in
  T.iter_triangles (
    fun p1 p2 p3 -> 
      faces := (p1, p2, p3)::!faces
  ) t;
  !faces

module Visualize =
struct
  open Scene

  let reshape ~width ~height =
    let open GL in
    let height = max height 1 in
    glViewport 0 0 width height;
    let aspect = float width /. float height in
    glMatrixMode GL_PROJECTION;
    glLoadIdentity();
    Glu.gluPerspective ~fovy:40.0 ~aspect ~zNear:0.5 ~zFar:1000.0;
    glMatrixMode GL_MODELVIEW

  let time_counter () =
    let t0 = ref None in
    fun () ->
      let t0' = 
	match !t0 with
	| None -> 
	  let t = Unix.gettimeofday () in
	  t0 := Some t;
	  t
	| Some t -> t
      in
      Unix.gettimeofday () -. t0'

  let fps_counter () =
    let t0 = ref None in
    let n = ref 0 in
    fun () ->
      let t0' = 
	match !t0 with
	| None -> 
	  let t = Unix.gettimeofday () in
	  t0 := Some t;
	  t
	| Some t -> t
      in
      let fps = float !n /. (Unix.gettimeofday () -. t0') in
      incr n;
      fps

  let count_fps = fps_counter ()

  let time = time_counter ()

  (* set camera position and lookat direction *)
  let set_camera ~posX ~posY ~posZ ~targetX ~targetY ~targetZ =
    let open GL in
    let open Glu in
    glMatrixMode GL_MODELVIEW;
    glLoadIdentity();
    gluLookAt posX posY posZ  targetX targetY targetZ  0. 1. 0. (* eye(x,y,z), focal(x,y,z), up(x,y,z) *)

  let display_mesh (((ofs_x, ofs_y, ofs_z)), (vertices, normals, colors)) () =
    let open GL in
    let open Glut in
    let open VBO in
    let open VertArray in
    (* Printf.printf "%.2f fps\n%!" (count_fps ()); *)

    set_camera ofs_x ofs_y ofs_z  0. 0. (0.);

    glMatrixMode GL_MODELVIEW;
    glPushMatrix ();
    glRotate (-90.0) 1.0 0.0 0.0;
    glRotate (30.0 *. time ()) 0.0 0.0 1.0;
    glClearColor 0.5 0.5 0.5 1.0;
    glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

    glEnableClientState GL_VERTEX_ARRAY;
    glEnableClientState GL_NORMAL_ARRAY;
    glEnableClientState GL_COLOR_ARRAY;
    glVertexPointer 3 Coord.GL_FLOAT 0 vertices;
    glNormalPointer Norm.GL_FLOAT 0 normals;
    glColorPointer 3 Color.GL_FLOAT 0 colors;
    (* glTranslate (~-. size /. 2.0) (~-. size /. 2.0) 0.0; *)
    glDrawArrays ~mode:GL_TRIANGLES ~first:0 ~count:(Bigarray.Array1.dim vertices / 3);
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState GL_NORMAL_ARRAY;
    glDisableClientState GL_COLOR_ARRAY;

    glPopMatrix();
    glutSwapBuffers()

  let program_init () =
    let vertexShaderSrc = "
#version 120
invariant gl_Position;

varying vec3 normal;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
  normal = normalize(gl_NormalMatrix * gl_Normal);
//  gl_FrontColor = gl_Color;
  gl_FrontColor = gl_Color;
//  gl_FrontColor = vec4(0.0, 0.0, 0.0, 0.0);
}
" in
    let fragmentShaderSrc = "
#version 120
varying vec3 normal;
void main() {
//  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
//  gl_FragColor = vec4(gl_Color.xyz, 0.5);
  vec3 lightDir = normalize(vec3(1.0, 1.0, 1.0));
  float intensity = max(dot(lightDir, normalize(normal)), 0.0);
//  float intensity = max(-normal.y, 0.0);
  gl_FragColor = vec4(intensity * gl_Color.xyz, 0.5);
//  gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
//  gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}
" in
    let open GL in
    let fragmentShader = glCreateShader GL_FRAGMENT_SHADER in
    let vertexShader = glCreateShader GL_VERTEX_SHADER in
    let program = glCreateProgram () in
    glShaderSource vertexShader vertexShaderSrc;
    glShaderSource fragmentShader fragmentShaderSrc;
    let compileShader label shader =
      glCompileShader shader;
      if not (glGetShaderCompileStatus shader) then
	failwith (label ^ ": " ^ glGetShaderInfoLog shader)
    in
    compileShader "vertexShader" vertexShader;
    compileShader "fragmentShader" fragmentShader;
    glAttachShader program fragmentShader;
    glAttachShader program vertexShader;
    glLinkProgram program;
    glUseProgram program

  let glut_init ~width ~height scene =
    let open Glut in
    ignore(glutInit Sys.argv);
    glutInitDisplayMode [GLUT_RGB; GLUT_ALPHA; GLUT_DOUBLE; GLUT_DEPTH];
    glutInitWindowPosition ~x:100 ~y:100;
    glutInitWindowSize ~width ~height;
    ignore(glutCreateWindow ~title:"Surface");
    glutDisplayFunc ~display:(display_mesh scene);
    glutReshapeFunc ~reshape:reshape;
    glutIdleFunc ~idle:glutPostRedisplay;
    ()

  let init_OpenGL ~width ~height =
    let open GL in
    glPolygonMode GL_FRONT GL_FILL;
    (* glPolygonMode GL_FRONT GL_LINE; *)
    glFrontFace GL_CCW;
    glDisable GL_CULL_FACE;
    glCullFace GL_BACK;
    glDisable GL_BLEND;
    glEnable GL_DEPTH_TEST;
    glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA

  let run (args, scene) =
    let (vertices, normals, colors) = bas_of_scene scene in
    let (width, height) = (1024, 1024) in
    glut_init ~width ~height (args, (vertices, normals, colors));
    reshape ~width ~height;
    Printf.printf "Enter\n%!";
    init_OpenGL ~width ~height;
    Glut.glutMainLoop ();
    Printf.printf "Leave\n%!";
    ()
end

let main () =
  let points_with_id = Array.map (add_id (gen_id ())) vertices in
  let triangulation = T.triangulate points_with_id in
  let faces = faces_of_triangulation triangulation in
  Printf.printf "%d faces\n%!" (List.length faces);

  let scale = 0.3 in
  let scene =
    List.fold_left 
      (fun scene face ->
	Scene.add face scene
      )
      Scene.empty
      (List.map
	 (fun (a, b, c) ->
	   let mk_v (_, ((x, y), z)) =
	     { RecVec.x = scale *. x; y = scale *. y; z = z }
	   in
	   let v1 = mk_v a in
	   let v2 = mk_v b in
	   let v3 = mk_v c in
	   Scene.(
	     face 
	       [vertex_of_recvec v1;
		vertex_of_recvec v2;
		vertex_of_recvec v3]
	       RecVec.(unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1)))
	       { RecVec.x = Random.float 1.0; y = Random.float 1.0; z = Random.float 1.0 }
	   )
	 )
	 faces)
  in
  Visualize.run (((0.0, 200.0, 400.0)), Scene.center_scene scene);

  ()

let _ = main ()
