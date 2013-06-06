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

module Scene =
struct
  type vector = {
    x : float;
    y : float;
    z : float;
  }

  let vector0 = { x = 0.0; y = 0.0; z = 0.0 }

  type ('normal, 'color) face = {
    vs : vector array;
    normal : 'normal;
    color : 'color;
  }

  type ('normal, 'color) scene = ('normal, 'color) face array

  let face0 normal color = { vs = [||]; normal = normal; color = color; }

  let pi = atan 1.0 *. 4.0

  let cross3 (a, b, c) (d, e, f) =
    (b *. f -. c *. e, 
     c *. d -. a *. f, 
     a *. e -. b *. d)

  let cross3' { x = a; y = b; z = c } { x = d; y = e; z = f } =
    { x = b *. f -. c *. e;
      y = c *. d -. a *. f;
      z = a *. e -. b *. d }

  let abs3' { x; y; z } = 
    sqrt (x ** 2.0 +. y ** 2.0 +. z ** 2.0)

  let (-.|) (x1, x2, x3) (y1, y2, y3) =
    (x1 -. y1, x2 -. y2, x3 -. y3)     

  let ( -.|. ) { x = x1; y = x2; z = x3 } { x = y1; y = y2; z = y3 } =
    { x = x1 -. y1; y = x2 -. y2; z = x3 -. y3 }

  let (+.|) (x1, x2, x3) (y1, y2, y3) =
    (x1 +. y1, x2 +. y2, x3 +. y3)     

  let ( +.|. ) { x = x1; y = x2; z = x3 } { x = y1; y = y2; z = y3 } =
    { x = x1 +. y1; y = x2 +. y2; z = x3 +. y3 }

  let ( *.| ) (x1, y1, z1) (x2, y2, z2) =
    (x1 *. x2, y1 *. y2, z1 *. z2)

  let ( *.|. ) { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
    { x = x1 *. x2; y = y1 *. y2; z = z1 *. z2 }

  let unit3' ({ x; y; z } as v) =
    if x <> 0.0 || y <> 0.0 || z <> 0.0 then
      let l = abs3' v in
      v *.|. { x = 1.0 /. l; y = 1.0 /. l; z = 1.0 /. l }
    else
      vector0

  let ba_of_array3 xs =
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
	  { vs = [|v1; v2; v3|];
	    normal = unit3' (cross3' (v3 -.|. v1) (v2 -.|. v1));
	    color = color_at x y; };
	let v1 = at (x + 0) (y + 1) in
	let v2 = at (x + 1) (y + 1) in
	let v3 = at (x + 1) (y + 0) in
	faces.(i + 1) <-
	  { vs = [|v1; v2; v3|];
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
	  let vertex = face.vs.(i mod 3) in
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

  let make_grid f scale width height =
    let size = width * height in 
    let ar = Bigarray.(Array1.create float32 c_layout (2 * 3 * 3 * size)) in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
	let i = 2 * 3 * 3 * (x + y * width) in
	let set_at i x y =
	  let x' = float x /. float width in
	  let y' = float y /. float height in
	  Bigarray.Array1.(
	    set ar (i + 0) (scale *. x');
	    set ar (i + 1) (scale *. y');
	    set ar (i + 2) (f x' y');
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

  let vertices =
    ba_of_array3
      [| 0.0,  0.0, 0.0;
  	 1.0,  0.0, 0.0;
  	 0.0,  1.0, 0.0|]

  let colors =
    ba_of_array3
      [| 0.0,  0.0, 1.0;
  	 1.0,  0.0, 0.0;
  	 0.0,  1.0, 0.0|]

  let triangle_mesh_normals verts_ba =
    let num_vertices = Bigarray.Array1.dim verts_ba / 3 in
    let vertex n =
      Bigarray.Array1.(
	get verts_ba (3 * n + 0),
	get verts_ba (3 * n + 1),
	get verts_ba (3 * n + 2)
      )
    in
    ba_of_array3 (
      Array.init num_vertices (
	fun vertex_idx ->
	  let v1 = vertex (3 * (vertex_idx / 3) + 0) in
	  let v2 = vertex (3 * (vertex_idx / 3) + 1) in
	  let v3 = vertex (3 * (vertex_idx / 3) + 2) in
	  cross3 (v2 -.| v1) (v3 -.| v1)
      )
    )
      

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
end

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

  let time = time_counter ()

  (* set camera position and lookat direction *)
  let set_camera ~posX ~posY ~posZ ~targetX ~targetY ~targetZ =
    let open GL in
    let open Glu in
    glMatrixMode GL_MODELVIEW;
    glLoadIdentity();
    gluLookAt posX posY posZ  targetX targetY targetZ  0. 1. 0. (* eye(x,y,z), focal(x,y,z), up(x,y,z) *)

  let display_mesh (size, (vertices, normals, colors)) () =
    let open GL in
    let open Glut in
    let open VBO in
    let open VertArray in
    (* Printf.printf "%.2f fps\n%!" (count_fps ()); *)

    set_camera 0.0 (10.0) 20.0  0. 0. (0.);

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
    glTranslate (~-. size /. 2.0) (~-. size /. 2.0) 0.0;
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

  let run (size, scene) =
    let (vertices, normals, colors) = bas_of_scene scene in
    let (width, height) = (1024, 1024) in
    glut_init ~width ~height (size, (vertices, normals, colors));
    reshape ~width ~height;
    Printf.printf "Enter\n%!";
    init_OpenGL ~width ~height;
    Glut.glutMainLoop ();
    Printf.printf "Leave\n%!";
    ()
end

let main () =
  let scale = 20.0 in
  let grid_width, grid_heigth = 50, 50 in
  (* let vertices = make_grid (fun x y -> sin (x *. pi) +. cos (2.0 *. y *. pi)) 10.0 grid_width grid_heigth *)
  (* let normals = triangle_mesh_normals vertices *)
  let scene = 
    Scene.(
      make_grid' (
	fun x y ->
	  (sin (x *. pi) +. cos (2.0 *. y *. pi), 
	   { x = (if mod_float (5.0 *. x) 1.0 > 0.5 then 1.0 else 0.0);
	     y = (if mod_float (5.0 *. y) 1.0 > 0.5 then 1.0 else 0.0);
	     z = 0.0 }
	  ))
	scale grid_width grid_heigth
    ) in
  let points_with_id = Array.map (add_id (gen_id ())) vertices in
  let triangulation = T.triangulate points_with_id in
  let faces = faces_of_triangulation triangulation in
  Printf.printf "%d faces\n%!" (List.length faces);

  Visualize.run (scale, (scene : (_, _) Scene.scene));

  ()

let _ = main ()
