open Graph

module FloatWithId = struct
  open Graph.Delaunay.FloatPoints

  type point = (int * (float * float))
      
  let ccw (_, p1) (_, p2) (_, p3) = ccw p1 p2 p3
  let in_circle (_, p1) (_, p2) (_, p3) (_, p4) = in_circle p1 p2 p3 p4      
end

module T = Graph.Delaunay.Make(FloatWithId)

let vertices = [|(20.791693,1037.017);(27.195517,409.9173);(197.4631,530.52356);(184.06242,743.35803);(222.68794,899.43671);(383.49622,714.19183);(557.7052,510.81668);(549.82239,854.50494);(700.38312,1000.3359);(691.71204,387.8457)|]

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
  let reshape ~width ~height =
    let open GL in
    let height = max height 1 in
    glViewport 0 0 width height;
    let aspect = float width /. float height in
    glMatrixMode GL_PROJECTION;
    glLoadIdentity();
    Glu.gluPerspective ~fovy:60.0 ~aspect ~zNear:0.5 ~zFar:1000.0;
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

  type context = { x : int }

  let ar_of_array3 xs =
    let ps = Bigarray.(Array1.create float32 c_layout (3 * Array.length xs)) in
    Array.iteri (
      fun i (x, y, z) ->
	Bigarray.Array1.(
	  set ps (i * 3 + 0) x;
	  set ps (i * 3 + 1) y;
	  set ps (i * 3 + 2) z
	)
    ) xs;
    ps

  let vertices = 
    ar_of_array3
      [| 0.0,  0.0, 2.1;
 	 1.0,  0.0, 2.1;
	 0.0,  1.0, 2.1|]

  let colors = 
    ar_of_array3
      [| 0.0,  0.0, 1.0;
 	 1.0,  0.0, 0.0;
	 0.0,  1.0, 0.0|]

  let time = time_counter ()

  (* set camera position and lookat direction *)
  let set_camera ~posX ~posY ~posZ ~targetX ~targetY ~targetZ =
    let open GL in
    let open Glu in
    glMatrixMode GL_MODELVIEW;
    glLoadIdentity();
    gluLookAt posX posY posZ  targetX targetY targetZ  0. 1. 0. (* eye(x,y,z), focal(x,y,z), up(x,y,z) *)

  let display_mesh context () =
    let open GL in
    let open Glut in
    let open VBO in
    let open VertArray in
    (* Printf.printf "%.2f fps\n%!" (count_fps ()); *)

    set_camera 0. 0. (-1.0 -. time ())  0. 0. (0.);

    glMatrixMode GL_MODELVIEW;
    glPushMatrix ();
    glTranslate 0.0 0.0 (-2.0);
    glClearColor 0.5 0.5 0.5 1.0;
    glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

    glEnableClientState GL_VERTEX_ARRAY;
    glEnableClientState GL_COLOR_ARRAY;
    glVertexPointer 3 Coord.GL_FLOAT 0 vertices;
    glColorPointer 3  Color.GL_FLOAT 0 colors;
    glDrawArrays ~mode:GL_TRIANGLES ~first:0 ~count:3;
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState GL_COLOR_ARRAY;

    glPopMatrix();
    glutSwapBuffers()

  let program_init () =
    let vertexShaderSrc = "
#version 130
in vec3 VertexPosition;
invariant gl_Position;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
  gl_FrontColor = gl_Color;
}
" in
    let fragmentShaderSrc = "
#version 130
precision highp float;
void main() {
//  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
  gl_FragColor = gl_Color;
}
" in
    let open GL in
    let fragmentShader = glCreateShader GL_FRAGMENT_SHADER in
    let vertexShader = glCreateShader GL_VERTEX_SHADER in
    let context = glCreateProgram () in
    glShaderSource vertexShader vertexShaderSrc;
    glShaderSource fragmentShader fragmentShaderSrc;
    glCompileShader fragmentShader;
    glCompileShader vertexShader;
    let compileShader label shader =
      if not (glGetShaderCompileStatus shader) then
	failwith (label ^ ": " ^ glGetShaderInfoLog shader)
    in
    compileShader "vertexShader" vertexShader;
    compileShader "fragmentShader" fragmentShader;
    glAttachShader context fragmentShader;
    glAttachShader context vertexShader;
    glLinkProgram context;
    glUseProgram context;

    { x = 0 }

  let glut_init ~width ~height =
    let open Glut in
    ignore(glutInit Sys.argv);
    glutInitDisplayMode [GLUT_RGB; GLUT_DOUBLE; GLUT_DEPTH];
    glutInitWindowPosition ~x:100 ~y:100;
    glutInitWindowSize ~width ~height;
    ignore(glutCreateWindow ~title:"Surface");
    let context = program_init () in
    glutDisplayFunc ~display:(display_mesh context);
    (* glutIdleFunc ~idle:glutPostRedisplay; *)
    glutIdleFunc ~idle:(display_mesh context);
    ()

  let init_OpenGL ~width ~height =
    let open GL in
    glDisable GL_DEPTH_TEST;
    glPolygonMode GL_FRONT GL_FILL;
    glFrontFace GL_CCW;    (* assume a clean model *)
    glDisable GL_CULL_FACE; (* deactivate elimination of polygons *)
    glCullFace GL_BACK;    (* remove back side of polygons *)
    ()

  let glut_run () =
    let (width, height) = (1024, 1024) in
    init_OpenGL ~width ~height;
    glut_init ~width ~height;
    reshape ~width ~height;
    Printf.printf "Enter\n%!";
    Glut.glutMainLoop ();
    Printf.printf "Leave\n%!";
    ()
end

let main () =
  let points_with_id = Array.map (add_id (gen_id ())) vertices in
  let triangulation = T.triangulate points_with_id in
  let faces = faces_of_triangulation triangulation in
  Printf.printf "%d faces\n%!" (List.length faces);

  Visualize.glut_run ();

  ()

let _ = main ()
