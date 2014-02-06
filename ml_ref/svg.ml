(*
 *  Modelica compiler API
 *
 *  Copyright (C) 2005 - 2007 Imagine S.A.
 *                2007 - 2010 LMS Imagine
 *  For more information or commercial use please contact us at www.lmsintl.com
 *
 *)

open XmlPrint
open Annotation

type transform =
  | Rotation of float * float * float
  | Translation of float * float
  | Scale of float * float
  | Matrix of matrix

and matrix = float * float * float * float * float * float

and tree = E of Xmlm.tag * tree list | D of string

module Buffer = Rope.Buffer

let evaluate x = Lazy.force x

(* Doing Trigonometry in Degrees, not Radians *)
let pi = acos(-. 1.);;
let degrees_of_radians r = 180. *. r /. pi;;
let radians_of_degrees d = d *. pi /. 180.;;

(* unit conversion *)
let unit_factor unit target_unit =
  let to_px unit =
    match unit with
    | "pt" -> 1.25
    | "pc" -> 15.
    | "mm" -> 3.543307
    | "cm" -> 35.43307
    | "in" -> 90.
    | _ -> 1. in
  to_px unit /. to_px target_unit

(* Placement point svg coordinates *)
let rec placement_point layer transf =
  let sys_orig = system_origin layer in
  match transf with
  | Some transf ->
      let x', y', w', h' =
        box_of transf.transf_origin transf.transf_extent in
      coordinate_of sys_orig { x = x' +. w' /. 2.; y = y' -. h' /. 2. }
  | None ->
      coordinate_of sys_orig { x = 0.; y = 0. }

and placement_transformation layer fscale g transf =
  match transf with
  | Some transf ->
      let sys_orig = system_origin layer in
      snd (resolve_transformation sys_orig fscale g transf)
  | None -> 1., 0., 0., 1., 0., 0.

and resolve_transformation sys_orig fscale g transf =
  let layer = g.Graphics.layer in
  let x', y', w', h' =
    box_of transf.transf_origin transf.transf_extent in
  let s = scale_of layer transf.transf_extent in
  let p = coordinate_of sys_orig { x = x'; y = y' } in
  let fx = if s.x >= 0. then 0. else w'
  and fy = if s.y >= 0. then 0. else h' in
  let o = coordinate_of sys_orig transf.transf_origin in
  let rot = Rotation (-. transf.transf_rotation, o.x, o.y)
  and tra = Translation (p.x +. fx, p.y +. fy)
  and sca = Scale (s.x, s.y) in
  2. *. fscale /. (abs_float s.x +. abs_float s.y),
  matrix_of_transformations [ rot; tra; sca ]

and system_origin layer =
  let x, y, _, _ = system_viewbox layer.coordinate_system in
  { x = x; y = y }

and system_viewbox : coordinate_system
                  -> (drawing_unit * drawing_unit * drawing_unit * drawing_unit)
  = fun coord_sys ->
  box_of { x = 0.; y = 0. } coord_sys.coordinate_system_extent

and coordinate_of : point -> point -> point
  = fun sys_orig p ->
  {
    x = p.x -. sys_orig.x;
    y = sys_orig.y -. p.y
  }

and box_of : point -> (point * point)
          -> (drawing_unit * drawing_unit * drawing_unit * drawing_unit)
  = fun orig (p, p') ->
  orig.x +. min p.x p'.x,
  orig.y +. max p.y p'.y,
  abs_float (p'.x -. p.x),
  abs_float (p'.y -. p.y)

and scale_of layer (p, p') =
  let coord_sys = layer.coordinate_system in
  let _, _, w, h =
    box_of { x = 0.; y = 0. } coord_sys.coordinate_system_extent in
  {
    x = if w != 0. then (p'.x -. p.x) /. w else 0.1;
    y = if h != 0. then (p'.y -. p.y) /. h else 0.1
  }

and matrix_of_transformations transfs =
  let multiply (a, b, c, d, e, f) (a', b', c', d', e', f') =
    a *. a' +. c *. b',
    b *. a' +. d *. b',
    a *. c' +. c *. d',
    b *. c' +. d *. d',
    a *. e' +. c *. f' +. e,
    b *. e' +. d *. f' +. f in
  let matrix_of_transformation transf =
    match transf with
    | Rotation (a, x, y) ->
        let a = radians_of_degrees a in
        let cosa = cos a
        and sina= sin a in
        let e = -. x *. cosa +. y *. sina +. x
        and f = -. x *. sina -. y *. cosa +. y in
        cosa, sina, -. sina, cosa, e, f
    | Translation (tx, ty) -> 1., 0., 0., 1., tx, ty
    | Scale (sx, sy) -> sx, 0., 0., sy, 0., 0.
    | Matrix (a, b, c, d, e, f) -> a, b, c, d, e, f in
  let matrices =
    List.map matrix_of_transformation transfs in
  let identity = 1., 0., 0., 1., 0., 0. in
  List.fold_left multiply identity matrices

and string_of_transformations transfs =
  let string_of_transformation transf =
    match transf with
    | Rotation (a, x, y) when a <> 0. ->
        Printf.sprintf "rotate(%.3g, %.3g, %.3g)" a x y
    | Translation (x, y) when x <> 0. || y <> 0. ->
        Printf.sprintf "translate(%.3g, %.3g)" x y
    | Scale (sx, sy) when sx <> 1. || sy <> 1. ->
        Printf.sprintf "scale(%.3g, %.3g)" sx sy
    | Matrix (1., 0., 0., 1., 0., 0.) -> ""
    | Matrix (a, b, c, d, e, f) ->
        Printf.sprintf
          "matrix (%.3g, %.3g, %.3g, %.3g, %.3g, %.3g)"
          a b c d e f
    | _ -> "" in
  String.concat " " (List.map string_of_transformation transfs)

(*****************************************************)
(* Gradient Definitions                              *)
(*****************************************************)

(** Little type used to describe required gradients
 * for icon rendering. *)
type required_gradient = (fill_pattern * (color * color))

(** Produce a CSS/SVG color definition *)
let color_of : color -> string
  = fun (RGB (r, g, b)) -> Printf.sprintf "rgb(%d, %d, %d)" r g b

(** Create an "HTML" code color, without the '#' *)
let color_code_of : (color * color) -> string
  = fun (RGB (r, g, b), RGB (r', g', b')) ->
        Printf.sprintf "%02X%02X%02X_%02X%02X%02X" r g b r' g' b'

(** Linear interpolation between two colors. *)
let lerp_color : color -> color -> float -> color
  = fun (RGB (ro, go, bo)) (RGB (rd, gd, bd)) zero_to_one ->
    let coeff_changer a b =
        let fa = float_of_int a
        and fb = float_of_int b
        in int_of_float (fa +. (fb -. fa) *. zero_to_one)
    in
    RGB (coeff_changer ro rd, coeff_changer go gd, coeff_changer bo bd)

(** For gradient used as Cylinder, hand rolling a gradient doesn't
 * really work, So let's apply a direct diffuse lighting algorithm to
 * generate it. Here, we are sampling elements on the unit sphere and use
 * the sinus of the position as color intensity
 *)
let prepare_circular_interpolation : int -> (color * color) -> (float * color) list
  = fun point_count (base_color, line_color) ->
    let unit_increment = 1.0 /. float_of_int point_count
    and circle_increment = 3.1415 /. float_of_int point_count
    and edge_color = if line_color = base_color
                then RGB (0,0,0) else line_color
    in
    let rec aux n = match n with
      | p when p > point_count -> []
      | p ->
          let position = float_of_int p *. unit_increment
          and circle_position = float_of_int p *. circle_increment in
          let new_color = lerp_color edge_color base_color (sin circle_position)
          in (position, new_color) :: aux (n + 1)
    in
    aux 0

(** Render all the stop point of a gradient in order to generate a
 * cylindrical looking gradient *)
let render_circular_interpolation : Rope.Buffer.t -> (color * color) -> unit
  = fun buffer color ->
    let sampling_point_count = 10 in
    let elem_renderer (pos, color) =
           add_empty_tag buffer "stop"
                [("offset", string_of_float pos) ;("stop-color", color_of color)]
    and point_list = prepare_circular_interpolation sampling_point_count color
    in
    List.iter elem_renderer point_list

(** Helper function for XML generation, create a tag and close it,
 * calling the argument function in-between. *)
let with_tag : Buffer.t -> string -> (string * string) list -> (Buffer.t -> unit) -> unit
  = fun buffer tag_name attribs f ->
    begin
        add_start_tag buffer tag_name attribs;
        f buffer;
        add_end_tag buffer tag_name
    end

(** Function used for the creation of a vertical horizontal gradient in the
 * "defs" part of the SVG document *)
let add_horizontal_cylinder_gradient_definition : Rope.Buffer.t -> (color * color) -> unit
  = fun buffer color ->
        with_tag buffer "linearGradient"
            [ ("id", "HorizontalCylinder_" ^ color_code_of color)
            ; ("x1", "0%") ; ("y1",   "0%")
            ; ("x2", "0%") ; ("y2", "100%")
            ]
            (fun _ -> render_circular_interpolation buffer color)


(** Function used for the creation of a vertical linear gradient in the
 * "defs" part of the SVG document *)
let add_vertical_cylinder_gradient_definition : Rope.Buffer.t -> (color * color) -> unit
  = fun buffer color ->
        with_tag buffer "linearGradient"
            [ ("id", "VerticalCylinder_" ^ color_code_of color)
            ; ("x1",   "0%") ; ("y1", "0%")
            ; ("x2", "100%") ; ("y2", "0%")
            ]
            (fun _ -> render_circular_interpolation buffer color)

(** Function used for the creation of a radial gradient in the
 * "defs" part of the SVG document *)
let add_spherical_gradient_definition : Rope.Buffer.t -> (color * color) -> unit
  = fun buffer color ->
    let (fill_color, line_color) = color in
    let str_color = color_of fill_color
    and str_interpolated_color =
        color_of (lerp_color line_color fill_color 0.3)
    in
    begin
        with_tag buffer "radialGradient" [("id", "SphereFilling_"  ^ color_code_of color)]
            (fun _ ->
                List.iter (add_empty_tag buffer "stop")
                    [ [ ("offset", "0%");   ("stop-color", str_color) ]
                    ; [ ("offset", "30%");  ("stop-color", str_color) ]
                    ; [ ("offset", "100%"); ("stop-color", str_interpolated_color) ]
                    ])
    end

(** Helper function to draw a line in SVG *)
let add_svg_line : Buffer.t -> (string * string) list -> (drawing_unit * drawing_unit)
                -> (drawing_unit * drawing_unit) -> color -> drawing_unit
                -> unit
  = fun buffer other_attribs (x1, y1) (x2, y2) color thickness ->
    let print_coord = Printf.sprintf "%.3g"
    in
      add_empty_tag buffer "line"
           ([ ("x1", print_coord x1)
            ; ("y1", print_coord y1)
            ; ("x2", print_coord x2)
            ; ("y2", print_coord y2)
            ; ("stroke", color_of color)
            ; ("stroke-width", Printf.sprintf "%.3gmm" thickness)
            ] @ other_attribs)

(** Generate a line pattern, always draw an horizontal line as basis for the
 * pattern :
 *
 * +---------+
 * |         |
 * +---------+
 * |         |
 * +---------+
 *
 * If a second line is required, it's drawed to form a cross :
 *
 *
 * +----+----+
 * |    |    |
 * +----+----+
 * |    |    |
 * +----+----+
 * 
 * The way to obtain the "diagonal" patterns is to apply a rotation
 * angle to the pattern, given in parameter.
 *)
let add_line_pattern : Buffer.t -> string -> (color * color) -> int -> bool -> unit
  = fun buffer name color angle with_secondary ->
    let (from, to_point) = ((0.0, 0.5), (1.0, 0.5))
    and (from_secondary, to_secondary) = ((0.5, 0.0), (0.5, 1.0))
    and print_coord = Printf.sprintf "%.3g"
    and scale = 2.5 in
    let scale_point (a, b) = (scale *. a, scale *. b)
    and (fill_color, line_color) = color
    in
    with_tag buffer "pattern" [ ("id", name ^ color_code_of color)
                              ; ("x", "0"); ("y", "0")
                              ; ("width", print_coord scale)
                              ; ("height", print_coord scale)
                              ; ("patternTransform", "rotate(" ^ string_of_int angle ^ ")")
                              ; ("patternUnits", "userSpaceOnUse")
                              ]
        (fun _ ->
          begin
              add_empty_tag buffer "rect" [("x", "0"); ("y", "0")
                                          ;("width", print_coord scale); ("height", print_coord scale)
                                          ;("fill", color_of fill_color)
                                          ];
            add_svg_line buffer [] (scale_point from) (scale_point to_point) line_color 0.1;
            if with_secondary
              then add_svg_line buffer []
                                (scale_point from_secondary)
                                (scale_point to_secondary) line_color 0.1
              else ()
          end)

(* This function call the various pattern generator for every encountered
 * pattern. Mostly a dispatch function, but contain data for the "line"
 * patterns. *)
let add_fill_pattern_definitions : Buffer.t -> required_gradient list -> unit
  = fun buffer lst ->
    let fill_pattern_dispatcher (pattern, color) = match pattern with
            | FillHorizontalCylinder ->
                add_horizontal_cylinder_gradient_definition buffer color
            | FillVerticalCylinder ->
                add_vertical_cylinder_gradient_definition buffer color
            | FillSphere ->
                add_spherical_gradient_definition buffer color
            | FillBackward ->
                add_line_pattern buffer "BackwardPattern_" color (-45) false
            | FillForward ->
                add_line_pattern buffer "ForwardPattern_" color 45 false
            | FillVertical ->
                add_line_pattern buffer "VerticalPattern_" color 90 false
            | FillHorizontal ->
                add_line_pattern buffer "HorizontalPattern_" color 0 false
            | FillCross ->
                add_line_pattern buffer "CrossPattern_" color 0 true
            | FillCrossDiag ->
                add_line_pattern buffer "CrossDiagPattern_" color 45 true

            | FillSolid -> ()
    in
    List.iter fill_pattern_dispatcher lst

let string_of_arrow : arrow -> string = function
        | ArrowOpen -> "open"
        | ArrowFilled -> "filled"
        | ArrowHalf -> "half"

let arrow_of_string : string -> arrow option = function
    | "open" -> Some ArrowOpen
    | "filled" -> Some ArrowFilled
    | "half" -> Some ArrowHalf
    | _ -> None

let parse_arrow : string option -> arrow option
  = fun str -> 
    let open_regexp = Str.regexp "^url(#ArrowOpen_"
    and filled_regexp = Str.regexp "^url(#ArrowFilled_"
    and half_regexp = Str.regexp "^url(#ArrowHalf_"
    and matching s r = Str.string_match r s 0
    in
    let str_matcher str = match str with (* refactor? *)
        | s when matching s open_regexp  -> Some ArrowOpen
        | s when matching s filled_regexp -> Some ArrowFilled
        | s when matching s half_regexp -> Some ArrowHalf
        | _ -> None
    in
    match str with
        | None -> None
        | Some a -> str_matcher a

(** This function is in charge of generating the SVG geometry used for
 * arrows.
 *)
let add_arrow : Buffer.t -> arrow -> float -> float -> color -> (float * point)
             -> unit
  = fun buffer arrow user_size line_size color (rotation, p) ->
        let size = 3.543307 *. user_size in
        let point x y = Printf.sprintf "%.3g,%.3g " x y

        (* After reading the SVG spec, it's the way marker should
         * be implemented, so let's be lazy and do the same *)
        and move_to_point =
            Printf.sprintf "translate(%.3g, %.3g) " p.x p.y
        and rotation =
            Printf.sprintf "rotate(%.3g) " (-. rotation)
        and centering =
            Printf.sprintf "translate(0, %.3g) " (size /. -2.0)
        in
        let transformation = 
            ("transform", move_to_point ^ rotation ^ centering)
        in match arrow with
        | ArrowHalf ->
            add_svg_line buffer [transformation]
                (-. size, 0.0) (0.0, size /. 2.0) color line_size

        | ArrowOpen -> begin
                add_svg_line buffer [transformation]
                        (0.0, 0.0) (size, size /. 2.0) color line_size; 
                add_svg_line buffer [transformation]
                        (0.0, size) (size, size /. 2.0) color line_size
            end

        | ArrowFilled -> add_empty_tag buffer "polygon"
            (transformation ::
             [ ("points ", point 0.0 0.0
                         ^ point size (size /. 2.0)
                         ^ point 0.0 size
                         )
             ; ("stroke", "none")
             ; ("fill", color_of color)
             ])

(** Helper function which given a shape, can give a
 * correct attribute with generated gradient and/or
 * patterns. *)
let shape_fill_of : shape -> (string * string) list
  = fun shape ->
    let color = (shape.shape_fill_color, shape.shape_line_color)
    in
    match shape.shape_fill_pattern with
    | None -> [ ("fill", "none") ]
    | Some FillHorizontalCylinder ->
        [ ("fill", "url(#HorizontalCylinder_" ^ color_code_of color ^ ")") ]
    | Some FillVerticalCylinder ->
        [ ("fill", "url(#VerticalCylinder_" ^ color_code_of color ^ ")") ]
    | Some FillSphere ->
        [ ("fill", "url(#SphereFilling_" ^ color_code_of color ^ ")") ]
    | Some FillSolid ->
        [ ("fill", color_of shape.shape_fill_color) ]

    (** Currently deactivated *)
    (*
    | Some FillBackward ->
        [ ("fill", "url(#BackwardPattern_" ^ color_code_of color ^ ")") ]
    | Some FillForward ->
        [ ("fill", "url(#ForwardPattern_" ^ color_code_of color ^ ")") ]
    | Some FillHorizontal ->
        [ ("fill", "url(#HorizontalPattern_" ^ color_code_of color ^ ")") ]
    | Some FillVertical ->
        [ ("fill", "url(#VerticalPattern_" ^ color_code_of color ^ ")") ]
    | Some FillCrossDiag ->
        [ ("fill", "url(#CrossDiagPattern_" ^ color_code_of color ^ ")") ]
    | Some FillCross ->
        [ ("fill", "url(#CrossPattern_" ^ color_code_of color ^ ")") ]
        *)
    | _ -> [ ("fill", color_of shape.shape_fill_color) ]

(** This function add two shapes, one for the border left and up, and one
 * for the border bottom and right. It's used to render border pattern,
 * with a "Windows 95" button style.
 *)
let add_complex_border : Buffer.t -> border_pattern -> point
                      -> (drawing_unit * drawing_unit) -> drawing_unit
                      -> unit
  = fun buffer pattern position (width, height) thickness ->
    let bright_color = RGB (200, 200, 200)
    and dark_color   = RGB ( 50,  50,  50)
    and shadow_color = RGB (128, 128, 128) in
    (*let over_border = thickness /. 2.0 in*)
    let draw_border up_color down_color =
      begin
        add_empty_tag buffer "polyline"
                [ ("points", Printf.sprintf "%.3g,%.3g %.3g,%.3g %.3g %.3g"
                                    position.x (position.y +. height)
                                    position.x  position.y
                                    (position.x +. width) position.y
                                    )
                ; ("fill", "none")
                ; ("stroke", color_of up_color)
                ; ("stroke-width", Printf.sprintf "%.3gmm" thickness)
                ];

        add_empty_tag buffer "polyline"
                [ ("points", Printf.sprintf "%.3g,%.3g %.3g,%.3g %.3g %.3g"
                                    position.x (position.y +. height)
                                    (position.x +. width) (position.y +. height)
                                    (position.x +. width) position.y
                                    )
                ; ("fill", "none")
                ; ("stroke", color_of down_color)
                ; ("stroke-width", Printf.sprintf "%.3gmm" thickness)
                ]
      end
    in
    match pattern with
     | BorderRaised -> draw_border bright_color dark_color
     | BorderSunken -> draw_border dark_color bright_color
     | BorderEngraved -> draw_border shadow_color dark_color

let string_of_border_pattern : border_pattern -> string
  = function | BorderRaised -> "raised"
             | BorderSunken -> "sunken"
             | BorderEngraved -> "engraved"

let border_pattern_of_string : string -> border_pattern option
  = function | "raised" -> Some BorderRaised
             | "sunken" -> Some BorderSunken
             | "engraved" -> Some BorderEngraved
             | _ -> None

(* This function translate the set of point for smooth
 * line/polygon to a path usable by SVG, the output string
 * contain data directly usable in the "d" attribute of
 * the path SVG tag. *)
let prepare_bezier_points : bool -> point list -> string
  = fun is_closed original_point_list ->
    let midpoint { x = x1; y = y1 } { x = x2; y = y2 } =
        { x = (x1 +. x2) /. 2.0; y = (y1 +. y2) /. 2.0 }
    and move p =
        Printf.sprintf "M %.3g %.3g" p.x p.y
    and bezier p1 p2 =
        Printf.sprintf "Q %.3g %.3g, %.3g %.3g" p1.x p1.y p2.x p2.y
    and lineto p =
        Printf.sprintf "L %.3g %.3g" p.x p.y in
    let rec compute_midpoints lst = match lst with
        | [] | [_] -> []
        | x :: ((x' :: _) as rest) ->
           midpoint x x' :: compute_midpoints rest
    in
    let rec produce_bezier_points ((orig, first_mid) as first_point) acc original_list midpoint_list =
        match (original_list, midpoint_list) with
          | ([] , _) -> List.rev acc (* this case shouldn't arrise *)
          | ([p], []) when is_closed -> List.rev (bezier orig first_mid :: acc)
          | ([p], []) -> List.rev (lineto p :: acc)
          | (_ :: rest, []) -> produce_bezier_points first_point acc rest []
          | (o :: os, b :: bs) ->
                  produce_bezier_points first_point (bezier o b :: acc) os bs
    in
    let setup_bezier_path bezier_list =
        match (original_point_list, bezier_list) with
        | (o :: os, b :: bs) when is_closed ->
                produce_bezier_points (o, b) [move b] os bs
        | (o :: os, b :: bs) ->
                produce_bezier_points (o, b) [lineto b; move o ] os bs
        | _ -> []
    in
    match original_point_list with
     | [] -> ""
     | [_] -> ""
     | [p1; p2] -> move p1 ^ lineto p2
     | _ -> let midpoints = compute_midpoints original_point_list in
            let points = setup_bezier_path midpoints
            in String.concat " " points

(** This function return a list of things without any duplicate.
 * /!\ run in O(n²)
 *)
let nub : 'a list -> 'a list
  = fun lst ->
    let unique_checker acc el =
        if List.mem el acc then acc else el :: acc
    in
    List.fold_left unique_checker [] lst

let is_gradient_shaded : fill_pattern option -> bool
  = fun p -> match p with
      | None -> false
      | Some FillSphere
      | Some FillVerticalCylinder
      | Some FillHorizontalCylinder -> true
      | Some _ -> false

(** This function search in all the graphical elements shapes which
 * require gradient display and extract the color information which
 * is required for SVG gradient creation.
 *
 * Each combination of pattern and color is assured to be given only
 * once.
 *)
let gather_fill_patterns : graphic_item list -> required_gradient  list
  = fun lst ->
      let rec color_extractor lst = match lst with
        | [] -> []
        (** Currently deactivated *)
        (*
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillBackward as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillForward as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillVertical as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillHorizontal as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillCross as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillCrossDiag as p) } as s) } :: rest
        *)
        | { graphic_item_nature = 
              Shape ({ shape_fill_pattern = Some (FillSphere as p) } as s)} :: rest
        | { graphic_item_nature = 
              Shape ({ shape_fill_pattern = Some (FillVerticalCylinder as p) } as s) } :: rest
        | { graphic_item_nature =
              Shape ({ shape_fill_pattern = Some (FillHorizontalCylinder as p) } as s) } :: rest
                -> (p, (s.shape_fill_color, s.shape_line_color)) :: color_extractor rest

        | _ :: xs -> color_extractor xs
      in
      nub (color_extractor lst)

(** Parse an SVG path, in case of parsing error, return
 * an empty list. *)
let parse_svg_path : string -> Svgpathparser.svg_path list
  = fun str ->
    let lexbuf = Lexing.from_string str
    in
    try
      Svgpathparser.svg_path Svgpathlexer.token lexbuf
    with Parsing.Parse_error -> []

(** Exception used to while importing some SVG path, shouldn't
 * normally escape top level function *)
exception InvalidSvgPathParsing

(** This function will try to import a svg point list to a smooth
 * modelica model. If it fail, it return None, otherwise Some and
 * the list of imported points. *)
let import_svg_smooth_curve : Svgpathparser.svg_path list -> (point list) option
  = fun path ->
    let rec curve_importer xs = match xs with
        | [Svgpathparser.LineTo (Svgpathparser.Absolute, [(x,y)] )] ->
                [{x = x; y = y}]
        | Svgpathparser.QuadraticBezier 
                (Svgpathparser.Absolute, [((x,y),_)]) :: rest ->
                { x = x; y = y } :: curve_importer rest
        | _ -> raise InvalidSvgPathParsing
    in
    let rec polygon_importer acc xs = match xs with
        | [] -> acc
        | [Svgpathparser.QuadraticBezier 
                (Svgpathparser.Absolute, [((x,y),_)])] ->
            let p = { x = x; y = y }
            (* We put the last point twice to be sure to get it in the
             * output as it was present in the first place *)
            in p :: List.rev (p :: p :: acc)
        | Svgpathparser.QuadraticBezier 
                (Svgpathparser.Absolute, [((x,y),_)]) :: rest ->
                polygon_importer ({ x = x; y = y } :: acc) rest
        | _ -> raise InvalidSvgPathParsing
    in
    match path with
      | (Svgpathparser.MoveTo (Svgpathparser.Absolute, [(x, y)])
            :: Svgpathparser.LineTo (Svgpathparser.Absolute, [_]) :: rest) ->
            (try Some ({ x = x; y = y } :: curve_importer rest)
             with InvalidSvgPathParsing -> None)
      | (Svgpathparser.MoveTo (Svgpathparser.Absolute, [_])
            :: (Svgpathparser.QuadraticBezier _ :: _ as rest)) ->
            (try Some (polygon_importer [] rest)
             with InvalidSvgPathParsing -> None)
      | _ -> None

(* Annotation graphics to svg conversion *)
let rec to_string ctx root_dir g fscale link =
  let b = Buffer.create 1000 in
  add_iso_header b;
  let layer = g.Graphics.layer
  and placs = g.Graphics.placements
  and cl_name = g.Graphics.class_name in
  let sys_orig = system_origin layer
  and _, _, w, h = system_viewbox layer.coordinate_system
  in
  add_start_tag b "svg" (svg_attributes cl_name w h);
  add_title b;
  add_desc b;

  with_tag b "defs" [] (fun _ ->
      begin 
          let needed_gradients = gather_fill_patterns layer.graphics
          in add_fill_pattern_definitions b needed_gradients;
      end);

  List.iter
    (add_graphic_item ctx root_dir b sys_orig fscale "")
    layer.graphics;

  if link then
    List.iter (add_linked_placement ctx root_dir b sys_orig fscale) placs
  else
    List.iter (add_placement ctx root_dir b sys_orig fscale) placs;

  add_end_tag b "svg";

  try Rope.to_string (Buffer.contents b) with _ -> ""

and svg_attributes id w h =
  [
    "id", id;
    "width", "100%";
    "height", "100%";
    "viewBox", Printf.sprintf "%.3g %.3g %.3g %.3g" 0. 0. w h;
    "version", "1.1";
    "baseProfile", "full";
    "preserveAspectRatio", "xMinYMin meet";
    "xmlns", "http://www.w3.org/2000/svg";
    "xmlns:xlink", "http://www.w3.org/1999/xlink"
  ]

and add_title b =
  add_start_tag b "title" [];
  add_value b "layer";
  add_end_tag b "title"

and add_desc b =
  add_start_tag b "desc" [];
  add_value b "SVG layer description generated from Modelica annotations";
  add_end_tag b "desc"

and add_linked_placement ctx root_dir b sys_orig fscale (id, (g, transf)) =
  let name = Printf.sprintf "../%s.html" g.Graphics.class_name in
  let title = Printf.sprintf "%s %s" g.Graphics.class_name id in
  add_start_tag b "a"
    [
      "xlink:href", name;
      "xlink:title", title;
      "target", "_parent"
    ];
  add_placement ctx root_dir b sys_orig fscale (id, (g, transf));
  add_end_tag b "a"

and add_placement ctx root_dir b sys_orig fscale (id, (g, transf)) =
  let add_placement' transf =
    let fscale, mat = resolve_transformation sys_orig fscale g transf in
    let tr = string_of_transformations [ Matrix mat ] in
    add_start_tag b "g" [ "id", id; "transform", tr ];
    let layer = g.Graphics.layer
    and placs = g.Graphics.placements in
    let sys_orig = system_origin layer in
    List.iter
      (add_placement_graphic_item ctx root_dir b sys_orig fscale id)
      layer.graphics;
    List.iter (add_placement ctx root_dir b sys_orig fscale) placs;
    add_end_tag b "g" in
  match transf with
  | Some transf -> add_placement' transf
  | None -> ()

and add_graphic_item ctx root_dir b sys_orig fscale id gr_item =
  let g = gr_item.graphic_item_origin
  and r = gr_item.graphic_item_rotation in
  let p = coordinate_of sys_orig g in
  (*let rot = Rotation (-. r, p.x, p.y) in
  let transfs =
    match gr_item.graphic_item_nature with
    | _ when g.x = 0. && g.y = 0. -> [ rot ]
    | Line _ | Shape { shape_nature = Polygon _ } ->
        [ rot; Translation (g.x, -. g.y) ]
    | _ -> [ rot ] in*)
  let mat = matrix_of_transformations [ Rotation (-. r, p.x, p.y) ] in
  match gr_item.graphic_item_nature with
  | Line line ->
      let line =
        {
          line with
          line_thickness = line.line_thickness *. fscale
        } in
      add_line b sys_orig gr_item mat line
  | Shape shape ->
      let shape =
        {
          shape with
          shape_line_thickness = shape.shape_line_thickness *. fscale
        } in
      add_shape b sys_orig id gr_item mat shape
  | Bitmap bitmap ->
      add_bitmap ctx root_dir b sys_orig gr_item mat bitmap

and add_placement_graphic_item ctx root_dir b sys_orig fscale id gr_item =
  add_graphic_item ctx root_dir b sys_orig fscale id gr_item
  (*match gr_item.graphic_item_nature with
  | Shape { shape_nature = Text _ } -> ()
  | _ -> add_graphic_item b sys_orig fscale gr_item*)

and add_bitmap ctx root_dir b sys_orig gr_item mat bitmap =
  let x, y, w, h =
    box_of gr_item.graphic_item_origin bitmap.bitmap_extent in
  let p = coordinate_of sys_orig { x = x; y = y } in
  add_empty_tag b "image"
    ([
      ("x", Printf.sprintf "%.3g" p.x);
      ("y", Printf.sprintf "%.3g" p.y);
      ("width", Printf.sprintf "%.3g" w);
      ("height", Printf.sprintf "%.3g" h);
      ("preserveAspectRatio", "xMinYMin");
      ("xlink:href", bitmap_data_of ctx root_dir bitmap)
     ] @
     visibility_of gr_item @
     transform_of mat)

and bitmap_data_of ctx root_dir bitmap =
  let resource_path s =
    match Filename.is_relative s with
    | true -> Filename.concat root_dir s
    | false -> s in
  match bitmap.bitmap_file_name with
  | "" ->
      Printf.sprintf "data:image/bitmap;base64,%s" bitmap.bitmap_image_source
  | s ->
      let info = Printf.sprintf "<html><img src=\"%s\"></html>" s
      and ref_dir = Scoping.context_relative_dirname (evaluate ctx) in
      let s =
        (match HtmlLexer.parse_html ref_dir (evaluate ctx) info with
         | _, s :: _ -> s
         | _ -> s) in
      resource_path s

and add_shape b sys_orig id gr_item mat shape =
  match shape.shape_nature with
  | Polygon poly -> add_polygon b sys_orig gr_item mat shape poly
  | Rectangle rect -> add_rectangle b sys_orig gr_item mat shape rect
  | Ellipse el -> add_ellipse b sys_orig gr_item mat shape el
  | Text text -> add_text b sys_orig id gr_item mat shape text

and add_line b sys_orig gr_item mat line =
  let orig = gr_item.graphic_item_origin in
  let prepare_point p =
      coordinate_of sys_orig { x = orig.x +. p.x; y = orig.y +. p.y } in
  let common_attributes = 
        [ ("fill", "none")
        ; ("stroke", color_of line.line_color)
        ; ("stroke-width", Printf.sprintf "%.3gmm" line.line_thickness)
        ; ("stroke-dasharray", stroke_dasharray_of line.line_pattern)
        ]
        @ visibility_of gr_item
        @ transform_of mat
  in
  let line_producer line =
        match line.line_smooth with
        | None -> add_empty_tag b "polyline"
            (("points", points_of sys_orig gr_item line.line_points) ::
                common_attributes)

        (* Currently deactivated *)
        | Some SmoothBezier -> add_empty_tag b "path"
            (("d", prepare_bezier_points false (List.map prepare_point line.line_points)) :: common_attributes)
  in
  let begin_vector = 
      let rec aux points = match points with
        | p1 :: ((p2 :: _) as rest) when p2.x = p1.x && p2.y = p1.y -> aux rest
        | p1 :: p2 :: _ -> ({ x = p1.x -. p2.x; y = p1.y -. p2.y }, prepare_point p1)
        | _ -> ({ x = 1.0; y = 0.0 }, { x = 1.0; y = 0.0 })
      in aux

  and end_vector points =
      let rec aux inv_points = match inv_points with
        | p2 :: ((p1 :: _) as rest) when p2.x = p1.x && p2.y = p1.y -> aux rest
        | (p2 :: p1 :: _) -> ({ x = p2.x -. p1.x; y = p2.y -. p1.y }, prepare_point p2)
        | _ -> ({ x = 1.0; y = 0.0 }, { x = 1.0; y = 0.0 })
      in aux (List.rev points)

  and calculate_angle (p, anchor) =
      let norm = sqrt (p.x *. p.x +. p.y *. p.y) in
      let cos_angle = p.x /. norm in
      let angle = acos cos_angle *. 180.0 /. pi
      in if p.y < 0.0
        then (-. angle, anchor)
        else (angle, anchor)
  in
  match line.line_arrow with
    | (  None,   None) -> line_producer line
    | (Some s, Some e) ->
         with_tag b "g" [("modelica_arrow_begin", string_of_arrow s)
                        ;("modelica_arrow_end", string_of_arrow e)]
            (fun _ ->
             let begin_angle = calculate_angle (begin_vector line.line_points)
             and end_angle = calculate_angle (end_vector line.line_points)
             and size = line.line_arrow_size
             and line_size = line.line_thickness
             in
             ( line_producer line
             ; add_arrow b s size line_size line.line_color begin_angle
             ; add_arrow b e size line_size line.line_color end_angle ))

    | (Some s,      _) ->
         with_tag b "g" [("modelica_arrow_end", string_of_arrow s)]
            (fun _ ->
             let begin_angle = calculate_angle (begin_vector line.line_points)
             in line_producer line;
                add_arrow b s (line.line_arrow_size) line.line_thickness line.line_color begin_angle)

    | (     _, Some e) ->
         with_tag b "g" [("modelica_arrow_end", string_of_arrow e)]
            (fun _ ->
             let end_angle = calculate_angle (end_vector line.line_points)
             in ( line_producer line
                ; add_arrow b e (line.line_arrow_size) line.line_thickness line.line_color end_angle))

and add_text b sys_orig id gr_item mat shape text =
  let text_string =
    match text.text_string with
    | "%name" -> id
    | _ when String.contains text.text_string '%' -> ""
    | _ -> text.text_string in
  let x, y, w, h =
    box_of gr_item.graphic_item_origin text.text_extent in
  let p = coordinate_of sys_orig { x = x; y = y } in
  let font_size =
    if text.font_size = 0. then h *. 0.6
    else text.font_size in
  let text_length =
    (font_size /. 2.) *. float_of_int (String.length text_string) in
  let start, text_anchor =
    match text.horizontal_alignment with
    | Left -> p, "start"
    | Center when text_length < w ->
        { x = p.x +. w /. 2.; y = p.y +. h /. 2. +. font_size *. 0.46 },
        "middle"
    | Center ->
        { x = p.x; y = p.y +. h /. 2. +. font_size *. 0.46 },
        "start"
    | Right -> { x = p.x +. w; y = p.y +. h }, "end" in
  match text_string with
  | "" -> ()
  | _ ->
      add_start_tag b "text"
        ([
          ("x", Printf.sprintf "%.3g" start.x);
          ("y", Printf.sprintf "%.3g" start.y);
          ("text-anchor", text_anchor);
          ("font-family", text.font_name);
          ("font-size", Printf.sprintf "%.3gpt" font_size);
          ("fill", color_of shape.shape_line_color);
          ("stroke", color_of shape.shape_line_color);
          ("stroke-width",
           Printf.sprintf "%.3gpt" shape.shape_line_thickness)
        ] @
        (visibility_of gr_item) @
        (text_style_attributes text.text_style) @
        transform_of mat);
      add_value b text_string;
      add_end_tag b "text"

and add_polygon b sys_orig gr_item mat shape poly =
  let stroking = if is_gradient_shaded shape.shape_fill_pattern
        then []
        else [ ("stroke", color_of shape.shape_line_color)
             ; ("stroke-width", Printf.sprintf "%.3gmm" shape.shape_line_thickness)
             ]
  in
  let common_attributes =
      shape_fill_of shape @ stroking @ visibility_of gr_item @ transform_of mat
  in match poly.polygon_smooth with
    | None ->
        let ps =
            match poly.polygon_points, List.rev poly.polygon_points with
            | p :: ps, p' :: (_ :: _) when p.x = p'.x && p.y = p'.y -> ps
            | ps, _ -> ps
        in add_empty_tag b "polygon"
            (("points", points_of sys_orig gr_item ps) :: common_attributes)
    (* Currently deactivated *)
    | Some SmoothBezier ->
        let orig = gr_item.graphic_item_origin in
        let prepare_point p =
                coordinate_of sys_orig { x = orig.x +. p.x; y = orig.y +. p.y } in
        let path_text =
                prepare_bezier_points true (List.map prepare_point poly.polygon_points)
        in add_empty_tag b "path" (("d", path_text) :: common_attributes)

and add_rectangle b sys_orig gr_item mat shape rect =
  let is_border_complex = rect.rectangle_border_pattern <> None in
  let stroking = if is_gradient_shaded shape.shape_fill_pattern
                        || is_border_complex 
        then []
        else [ ("stroke", color_of shape.shape_line_color)
             ; ("stroke-width"
               , Printf.sprintf "%.3gmm" shape.shape_line_thickness) ] in
  let (x, y, w, h) =
        box_of gr_item.graphic_item_origin rect.rectangle_extent in
  let p = coordinate_of sys_orig { x = x; y = y } in
  match rect.rectangle_border_pattern with
    | None ->
        add_empty_tag b "rect"
                ([
                    ("x", Printf.sprintf "%.3g" p.x);
                    ("y", Printf.sprintf "%.3g" p.y);
                    ("width", Printf.sprintf "%.3g" w);
                    ("height", Printf.sprintf "%.3g" h);
                    ("rx", Printf.sprintf "%.3g" rect.rectangle_radius);
                    ("ry", Printf.sprintf "%.3g" rect.rectangle_radius);
                ] @
                (shape_fill_of shape) @ stroking @
                visibility_of gr_item @
                transform_of mat)

    | Some pattern ->
        with_tag b "g" [("modelica_border", string_of_border_pattern pattern)]
                (fun _ ->
                    add_rectangle b sys_orig gr_item mat shape
                                { rect with rectangle_border_pattern = None };
                    add_complex_border b pattern p (w, h)
                                        shape.shape_line_thickness)

and add_ellipse b sys_orig gr_item mat shape el =
  let stroking = if is_gradient_shaded shape.shape_fill_pattern
        then []
        else [ ("stroke", color_of shape.shape_line_color)
             ; ("stroke-width"
               , Printf.sprintf "%.3gmm" shape.shape_line_thickness) ]
  and x, y, w, h =
    box_of gr_item.graphic_item_origin el.ellipse_extent in
  let p = coordinate_of sys_orig { x = x; y = y } in
  add_empty_tag b "ellipse"
    ([
      ("cx", Printf.sprintf "%.3g" (p.x +. w /. 2.));
      ("cy", Printf.sprintf "%.3g" (p.y +. h /. 2.));
      ("rx", Printf.sprintf "%.3g" (w /. 2.));
      ("ry", Printf.sprintf "%.3g" (h /. 2.));
     ] @
     shape_fill_of shape @
     stroking @
     visibility_of gr_item @
     transform_of mat)

and points_of : point -> graphic_item -> point list -> string
  = fun sys_orig gr_item ps ->
  let orig = gr_item.graphic_item_origin in
  let point_of p =
    let p' =
      coordinate_of sys_orig { x = orig.x +. p.x; y = orig.y +. p.y } in
    Printf.sprintf "%.3g,%.3g" p'.x p'.y in
  String.concat " " (List.map point_of ps)

and stroke_dasharray_of line_pattern =
  match line_pattern with
  | None | Some LineSolid -> ""
  | Some LineDash -> "5, 5"
  | Some LineDot -> "1, 2"
  | Some LineDashDot -> "5, 2, 1, 2"
  | Some LineDashDotDot -> "5, 2, 1, 2, 1, 2"

and visibility_of gr_item =
  match gr_item.graphic_item_visible with
  | true -> []
  | false -> [ "visibility", "hidden" ]

and transform_of mat =
  match string_of_transformations [ Matrix mat ] with
  | "" -> []
  | tr -> [ "transform", tr ]

and text_style_attributes text_styles =
  let text_style_of acc text_style =
    match text_style with
    | Italic -> ("font-style", "italic") :: acc
    | Bold -> ("font-weight", "bold") :: acc
    | Underline -> ("text-decoration", "underline") :: acc in
  List.fold_left text_style_of [] text_styles

and sign f = if f < 0. then -. 1. else 1.

and transform (a, b, c, d, e, f) p =
  {
    x = a *. p.x +. c *. p.y +. e;
    y = b *. p.x +. d *. p.y +. f
  }


(***********************************************)
(* Conversion from svg to Modelica annotations *)
(***********************************************)
let resolve_extent origin p q =
  { x = p.x -. origin.x; y = p.y -. origin.y },
  { x = q.x -. origin.x; y = q.y -. origin.y }

(** Color parsing is now more complex, we can obtain
 * gradient (which define two colors), fill pattern
 * (again with two colors) and solid colors. So this
 * type is here to transmit this information *)
type extracted_color =
    | SolidColor of color
    | PatternColor of fill_pattern * color * color

(** Extract the background, primary color of an extracted_color *)
let extract_primary_color : extracted_color -> color
  = fun extracted -> match extracted with
  | SolidColor c -> c
  | PatternColor (_, c, _) -> c


let fill_pattern_list : (string * fill_pattern) list =
    [ ("HorizontalCylinder", FillHorizontalCylinder)
    ; ("VerticalCylinder", FillVerticalCylinder)
    ; ("SphereFilling", FillSphere)
    ; ("BackwardPattern", FillBackward)
    ; ("ForwardPattern", FillForward)
    ; ("HorizontalPattern", FillHorizontal)
    ; ("VerticalPattern", FillVertical)
    ; ("CrossDiagPattern", FillCrossDiag)
    ; ("CrossPattern", FillCross) ]

let resolve_color : color -> string -> extracted_color 
 = fun default str ->
   let hex = "[a-fA-F0-9]" in
   let rgb_matcher =
       let num = "\\([1-9]+\\)[ \\t]*"
       and comma = ",[ \\t]*"
       in
       Str.regexp ("^rgb[ \\t]*([ \\t]*" ^ num ^ comma ^ num ^ comma ^ num ^ ")$")

   and color_match = "\\(" ^ hex ^ hex ^ "\\)" in
   let rgb_color_match = color_match ^ color_match ^ color_match in
   let full_html_color = Str.regexp ("^#" ^ rgb_color_match  ^ "$")

   and light_html_color =
       let hex_group = "\\(" ^ hex ^ "\\)"
       in Str.regexp ("^#" ^ hex_group ^ hex_group ^ hex_group ^ "$")

   and int_of_hexadecimal_string s =
     int_of_string ("0x" ^ s)
   
   and fill_pattern_matcher pat =
       Str.regexp ("^url(#\\(" ^ pat ^ "\\)_" ^ rgb_color_match ^ "_" ^ rgb_color_match) in

   let fill_pattern_matcher =
       List.map (fun (r, _) -> fill_pattern_matcher r) fill_pattern_list

   and matching r s = 
       Str.string_match r s 0 in

   let is_matching_pattern s =
       List.exists (fun r -> matching r s) fill_pattern_matcher

   in match str with
   | s when matching full_html_color s ->
       let group i = int_of_hexadecimal_string (Str.matched_group i s)
       in SolidColor (RGB (group 1, group 2, group 3))
       
   | s when matching light_html_color s ->
        let group i =
            let v = int_of_hexadecimal_string (Str.matched_group i s)
            in v * 16 + v
        in SolidColor (RGB (group 1, group 2, group 3))
   
    | s when matching rgb_matcher s ->
            let group i = int_of_string (Str.matched_group i s)
        in SolidColor (RGB (group 1, group 2, group 3))
   
    | s when is_matching_pattern s ->
        let group i = int_of_string ("0x" ^ (Str.matched_group (i+1) s))
        (* By definition safe, we use the list to build the regexp list. *)
        and pattern = List.assoc (Str.matched_group 1 s) fill_pattern_list 
        in PatternColor ( pattern
                        , RGB (group 1, group 2, group 3)
                        , RGB (group 4, group 5, group 6) )

    | _ -> SolidColor default

let resolve_string_attribute : 'a -> string -> ((string * string) * 'a) list -> 'a
  = fun default name attribs ->
    try List.assoc ("", name) attribs
    with Not_found -> default

let find_attribute : string -> ((string * string) * 'a) list -> 'a option
  = fun name attribs ->
    try Some (List.assoc ("", name) attribs)
    with Not_found -> None

let resolve_line_color attribs =
  let color = resolve_string_attribute "" "stroke" attribs
  in resolve_color blue color

let rec to_transformation layer layer' mat =
  let x, y, _, _ = system_viewbox layer.coordinate_system in
  let sys_orig = { x = -. x; y = y } in
  let x', y', w', h' = system_viewbox layer'.coordinate_system in
  let cx, cy, rot, p, q = matrix_factorization (0., 0., w', h') mat in
  let origin = coordinate_of sys_orig { x = cx; y = cy }
  and p = coordinate_of sys_orig p
  and q = coordinate_of sys_orig q in
  {
    Annotation.transf_origin = origin;
    Annotation.transf_extent = resolve_extent origin p q;
    Annotation.transf_rotation = -. degrees_of_radians rot
  }

(*and matrix_factorization (ox, oy, w, h) (a, b, c, d, e, f) =
  let rot, sx, sy, cosrot, sinrot =
    try
      if a = 0. then pi /. 2., b, -. c, 0., 1.
      else
        begin
          let rot = atan (b /. a) in
          let cosrot = cos rot
          and sinrot = sin rot in
          rot, a /. cosrot, d /. cosrot, cosrot, sinrot
        end
    with _ -> pi /. 2., b, -. c, 0., 1. in
  let w' = abs_float (sx *. w)
  and h' = abs_float (sy *. h) in
  let fx = w' *. phi (sx < 0.)
  and fy = h' *. phi (sy < 0.) in
  let alpha = -. w' /. 2. +. fx
  and beta = -. h' /. 2. +. fy in
  let cx = e -. alpha *. cosrot +. beta *. sinrot
  and cy = f -. alpha *. sinrot -. beta *. cosrot in
  let wx = (-. 1. +. 2. *. phi (sy < 0.)) *. w' /. 2.
  and hy = (-. 1. +. 2. *. phi (sx < 0.)) *. h' /. 2. in
  let p = transform (a, b, c, d, e, f) { x = ox; y = oy }
  and q = transform (a, b, c, d, e, f) { x = ox +. w; y = oy +. h } in
  let cx' = (p.x +. q.x) /. 2.
  and cy' = (p.y +. q.y) /. 2. in
  cx,
  cy,
  rot,
  { x = cx' +. wx; y = cy' -. hy },
  { x = cx' -. wx; y = cy' +. hy }

and phi b =
  if b then 1. else 0.*)

and matrix_factorization (ox, oy, w, h) (a, b, c, d, e, f) =
  let rot, sx, sy =
    try
      if a = 0. then pi /. 2., b, -. c
      else
        begin
          let rot = atan (b /. a) in
          let cosrot = cos rot in
          rot, a /. cosrot, d /. cosrot
        end
    with _ -> pi /. 2., b, -. c in
  let p1 = transform (a, b, c, d, e, f) { x = ox; y = oy }
  and p2 = transform (a, b, c, d, e, f) { x = ox +. w; y = oy }
  and p3 = transform (a, b, c, d, e, f) { x = ox +. w; y = oy +. h }
  and p4 = transform (a, b, c, d, e, f) { x = ox; y = oy +. h } in
  let cx = (p1.x +. p3.x) /. 2.
  and cy = (p1.y +. p3.y) /. 2.
  and w' = (distance p1 p2) *. sign sx
  and h' = (distance p1 p4) *. sign sy in
  cx,
  cy,
  rot,
  { x = cx -. w' /. 2.; y = cy +. h' /. 2. },
  { x = cx +. w' /. 2.; y = cy -. h' /. 2. }

and distance p q =
  let dx = q.x -. p.x
  and dy = q.y -. p.y in
  sqrt (dx *. dx +. dy *. dy)

and sign x =
  if x > 0. then 1. else if x < 0. then -. 1. else 0.

(* Conversion from svg string to Modelica layer *)
let rec to_layer svg =
  let i = Xmlm.make_input (`String (0, svg)) in
  match in_tree i with
  | _, D _ -> assert false
  | _, E (((_, "svg"), attribs), elts) ->
      let coord_sys, sys_orig = resolve_coordinate_system attribs in
      {
        Annotation.coordinate_system = coord_sys;
        Annotation.graphics = resolve_svg_elements sys_orig coord_sys elts
      }
  | _ -> assert false

(* Conversion of an svg graphic to Modelica using a given layer coordinate system *)
and resolve_graphics coord_sys svg =
  let x, y, _, _ = system_viewbox coord_sys in
  let sys_orig = { x = -. x; y = y } in
  let i = Xmlm.make_input (`String (0, svg)) in
  match in_tree i with
  | _, D _ -> assert false
  | _, E (((_, "svg"), attribs), elts) ->
      resolve_svg_elements sys_orig coord_sys elts
  | _ -> assert false

and resolve_coordinate_system attribs =
  let viewbox =
    resolve_string_attribute "0. 0. 200. 200." "viewBox" attribs in
  let x, y, w, h =
    match Str.split (Str.regexp "[, \t\r]+") viewbox with
    | [ x; y; w; h ] ->
        float_of_string x,
        float_of_string y,
        float_of_string w,
        float_of_string h
    | _ -> 0., 0., 200., 200. in
  let sys_orig = { x = x +. w /. 2.; y = y +. h /. 2. } in
  let p = coordinate_of sys_orig { x = x; y = y +. h }
  and p' = coordinate_of sys_orig { x = x +. w; y = y } in
  {
    Annotation.default_coordinate_system with
    Annotation.coordinate_system_extent = p, p'
  },
  sys_orig

and resolve_svg_elements sys_orig coord_sys elts =
  List.fold_left (resolve_svg_element sys_orig coord_sys) [] elts

and resolve_svg_element sys_orig coord_sys acc elt =
  match elt with
  | D _ -> acc
  | E (((_, "path"), attribs), _) ->
      acc @ (resolve_path sys_orig coord_sys attribs)
  | E (((_, "text"), attribs), elts) ->
      acc @ [ resolve_text sys_orig coord_sys attribs elts ]
  | E (((_, "polyline"), attribs), _) ->
      acc @ [ resolve_polyline None sys_orig coord_sys attribs ]
  | E (((_, "polygon"), attribs), _) ->
      acc @ [ resolve_polygon sys_orig coord_sys attribs ]
  | E (((_, "rect"), attribs), _) ->
      acc @ [ resolve_rect sys_orig coord_sys attribs ]
  | E (((_, "ellipse"), attribs), _) ->
      acc @ [ resolve_ellipse sys_orig coord_sys attribs ]
  | E (((_, "image"), attribs), _) ->
      acc @ [ resolve_image sys_orig coord_sys attribs ]
    (* modelica_border imply rectangle element *)
  | E (((_, "g"), attribs), children) when find_attribute "modelica_border" attribs <> None ->
          (match resolve_svg_elements sys_orig coord_sys children with
            | { graphic_item_nature =
                    Shape ({ shape_nature = Rectangle r } as s) } as item :: _->

                let string_border =
                    resolve_string_attribute "" "modelica_border" attribs in
                let pattern =
                    border_pattern_of_string string_border in
                let new_rect =
                    Rectangle { r with rectangle_border_pattern = pattern } in
                let new_shape =
                    Shape { s with shape_nature = new_rect }
                in acc @ [{ item with graphic_item_nature = new_shape }]

            | _ -> acc)
  | E (((_, "g"), attribs), child :: _) when
            (find_attribute "modelica_arrow_begin" attribs <> None ||
                find_attribute "modelica_arrow_end" attribs <> None) ->
        (match resolve_svg_elements sys_orig coord_sys [child] with
            | [{ graphic_item_nature = Line l } as item] ->
                let begin_attrib =
                    find_attribute "modelica_arrow_begin" attribs 
                and end_attrib = 
                    find_attribute "modelica_arrow_end" attribs in
                let begin_arrow = match begin_attrib with
                    | Some s -> arrow_of_string s
                    | _ -> None
                and end_arrow = match end_attrib with
                    | Some s -> arrow_of_string s
                    | _ -> None
                in
                let new_line =
                    Line { l with line_arrow = (begin_arrow, end_arrow) }
                in acc @ [{ item with graphic_item_nature = new_line }]

            | _ -> acc)
                

  | _ -> acc

and resolve_path sys_orig coord_sys attribs =
  let toPoint (x,y) = { x = x; y = y } in
  let rec resolve_path' smoothing path =
    match path, List.rev path with
    | p :: _, p' :: path when p.x = p'.x && p.y = p'.y ->
        resolve_closed_path smoothing sys_orig coord_sys attribs (List.rev path)
    | _ -> resolve_unclosed_path smoothing sys_orig coord_sys attribs path

  and resolve_path_elements (acc, last_path) elt =
    match elt with
    | Svgpathparser.MoveTo (_, [p]) ->
        (add_path acc last_path, [toPoint p])

    | Svgpathparser.LineTo (_, [p]) ->
        (acc, toPoint p :: last_path)

    | Svgpathparser.HorizontalTo (_, [x]) ->
        (acc, add_horizontal_point last_path x)

    | Svgpathparser.VerticalTo (_, [x]) ->
        (acc, add_vertical_point last_path x)

    | Svgpathparser.EndPath ->
        (add_path acc (close_path last_path), [])

    | _ -> (acc, last_path)

  and close_path path =
    match List.rev path with
    | [] -> []
    | p :: _ -> p :: path
  and add_horizontal_point path elt =
    match path with
    | [] -> []
    | p :: _ -> { x = elt; y = p.y } :: path
  and add_vertical_point path elt =
    match path with
    | [] -> []
    | p :: _ -> { x = p.x; y = elt } :: path
  and add_path acc path =
    match path with
    | [] -> acc
    | _ -> (List.rev path) :: acc in
  let d = resolve_string_attribute "" "d" attribs in
  let path_elts = parse_svg_path d
  in
  match import_svg_smooth_curve path_elts with
    | Some path -> [resolve_path' (Some SmoothBezier) path]
    | None ->
        let (acc, last_path) = List.fold_left resolve_path_elements ([], []) path_elts in
        let paths = List.rev (add_path acc last_path)
        in
        List.map (resolve_path' None) paths

and resolve_text sys_orig coord_sys attribs elts =
  let rec data_of elts =
    match elts with
    | [] -> ""
    | (D s) :: _ -> s
    | _ :: elts -> data_of elts in
  let align = resolve_text_alignment attribs
  and font_size = resolve_string_attribute "0." "font-size" attribs in
  let font_size = resolve_length "pt" 0. font_size in
  let x, y, w, h = resolve_text_viewbox attribs align font_size in
  let orig, rot, p, q = resolve_transform sys_orig (x, y, w, h) attribs in
  let sy = if h != 0. then (q.y -. p.y) /. h else 1. in
  let text =
    {
      text_extent = resolve_extent orig p q;
      text_string = data_of elts;
      font_size = font_size *. abs_float sy;
      font_name = resolve_font_family attribs;
      text_style = resolve_text_style attribs;
      horizontal_alignment = align
    } in
  let shape = resolve_shape (Text text) attribs in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = orig;
    graphic_item_rotation = rot;
    graphic_item_nature = Shape shape
  }

and resolve_unclosed_path smoothing sys_orig coord_sys attribs points =
  let mat = resolve_string_attribute "" "transform" attribs in
  let mat = resolve_matrix mat in
  let points = List.map (transform mat) points in
  (*let x, y, w, h = points_viewbox points in
  let orig, rot, _, _ = resolve_transform sys_orig (x, y, w, h) attribs in*)
  let line_points = List.map (coordinate_of sys_orig) points in
  (*let relative_point p = { x = p.x -. orig.x; y = p.y -. orig.y } in*)
  let color_info = resolve_line_color attribs in
  let line =
    {
      line_points = (*List.map relative_point*) line_points;
      line_color = extract_primary_color color_info;
      line_pattern = resolve_line_pattern attribs;
      line_thickness = resolve_line_thickness "mm" attribs;
      line_arrow = ( parse_arrow (find_attribute "marker-start" attribs)
                   , parse_arrow (find_attribute "marker-end" attribs));
      line_arrow_size = 3.;
      line_smooth = smoothing
    } in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = { x = 0.; y = 0. };
    graphic_item_rotation = 0.;
    graphic_item_nature = Line line
  }

and resolve_closed_path smoothing sys_orig coord_sys attribs points =
  let mat = resolve_string_attribute "" "transform" attribs in
  let mat = resolve_matrix mat in
  let points = List.map (transform mat) points in
  (*let x, y, w, h = points_viewbox points in
  let orig, rot, _, _ = resolve_transform sys_orig (x, y, w, h) attribs in*)
  let line_points = List.map (coordinate_of sys_orig) points in
  (*let relative_point p = { x = p.x -. orig.x; y = p.y -. orig.y } in*)
  let polygon =
    {
      polygon_points = (*List.map relative_point*) line_points;
      polygon_smooth = smoothing
    } in
  let shape = resolve_shape (Polygon polygon) attribs in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = { x = 0.; y = 0. };
    graphic_item_rotation = 0.;
    graphic_item_nature = Shape shape
  }

and resolve_polyline smoothing sys_orig coord_sys attribs =
  let points = resolve_points attribs in
  let mat = resolve_string_attribute "" "transform" attribs in
  let mat = resolve_matrix mat in
  let points = List.map (transform mat) points in
  (*let x, y, w, h = points_viewbox points in
  let orig, rot, _, _ = resolve_transform sys_orig (x, y, w, h) attribs in*)
  let line_points = List.map (coordinate_of sys_orig) points in
  (*let relative_point p = { x = p.x -. orig.x; y = p.y -. orig.y } in*)
  let color = resolve_line_color attribs in
  let line =
    {
      line_points = (*List.map relative_point*) line_points;
      line_color = extract_primary_color color;
      line_pattern = resolve_line_pattern attribs;
      line_thickness = resolve_line_thickness "mm" attribs;
      line_arrow = ( parse_arrow (find_attribute "marker-start" attribs)
                   , parse_arrow (find_attribute "marker-end" attribs));
      line_arrow_size = 3.;
      line_smooth = smoothing
    } in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = { x = 0.; y = 0. };
    graphic_item_rotation = 0.;
    graphic_item_nature = Line line
  }

and resolve_polygon sys_orig coord_sys attribs =
  let points = resolve_points attribs in
  let mat = resolve_string_attribute "" "transform" attribs in
  let mat = resolve_matrix mat in
  let points = List.map (transform mat) points in
  (*let x, y, w, h = points_viewbox points in
  let orig, rot, _, _ = resolve_transform sys_orig (x, y, w, h) attribs in*)
  let line_points = List.map (coordinate_of sys_orig) points in
  (*let relative_point p = { x = p.x -. orig.x; y = p.y -. orig.y } in*)
  let polygon =
    {
      polygon_points = (*List.map relative_point*) line_points;
      polygon_smooth = None
    } in
  let shape = resolve_shape (Polygon polygon) attribs in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = { x = 0.; y = 0. };
    graphic_item_rotation = 0.;
    graphic_item_nature = Shape shape
  }

and resolve_rect sys_orig coord_sys attribs =
  let x = resolve_float_attribute 0. "x" attribs
  and y = resolve_float_attribute 0. "y" attribs
  and w = resolve_float_attribute 0. "width" attribs
  and h = resolve_float_attribute 0. "height" attribs in
  let orig, rot, p, q = resolve_transform sys_orig (x, y, w, h) attribs in
  let rect =
    {
      rectangle_border_pattern = None;
      rectangle_extent = resolve_extent orig p q;
      rectangle_radius = resolve_float_attribute 0. "rx" attribs
    } in
  let shape = resolve_shape (Rectangle rect) attribs in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = orig;
    graphic_item_rotation = rot;
    graphic_item_nature = Shape shape
  }

and resolve_image sys_orig coord_sys attribs =
  let x = resolve_float_attribute 0. "x" attribs
  and y = resolve_float_attribute 0. "y" attribs
  and w = resolve_float_attribute 0. "width" attribs
  and h = resolve_float_attribute 0. "height" attribs in
  let orig, rot, p, q = resolve_transform sys_orig (x, y, w, h) attribs in
  let bitmap =
    {
      bitmap_extent = resolve_extent orig p q;
      bitmap_file_name = resolve_string_attribute "" "xlink:href" attribs;
      bitmap_image_source = ""
    } in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = orig;
    graphic_item_rotation = rot;
    graphic_item_nature = Bitmap bitmap
  }

and resolve_ellipse sys_orig coord_sys attribs =
  let cx = resolve_float_attribute 0. "cx" attribs
  and cy = resolve_float_attribute 0. "cy" attribs
  and rx = resolve_float_attribute 0. "rx" attribs
  and ry = resolve_float_attribute 0. "ry" attribs in
  let w = 2. *. rx
  and h = 2. *. ry
  and x = cx -. rx
  and y = cy -. ry in
  let orig, rot, p, q = resolve_transform sys_orig (x, y, w, h) attribs in
  let ellipse =
    {
      ellipse_extent = resolve_extent orig p q;
      ellipse_start_angle = 0.;
      ellipse_end_angle = 360.
    } in
  let shape = resolve_shape (Ellipse ellipse) attribs in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = orig;
    graphic_item_rotation = rot;
    graphic_item_nature = Shape shape
  }

and resolve_transform sys_orig (x, y, w, h) attribs =
  let mat = resolve_string_attribute "" "transform" attribs in
  let cx, cy, rot, p, q =
    matrix_factorization (x, y, w, h) (resolve_matrix mat) in
  coordinate_of sys_orig { x = cx; y = cy },
  -. degrees_of_radians rot,
  coordinate_of sys_orig p,
  coordinate_of sys_orig q

and resolve_graphic_item sys_orig viewbox nature attribs =
  let mat = resolve_string_attribute "" "transform" attribs in
  let cx, cy, rot, p, q =
    matrix_factorization viewbox (resolve_matrix mat) in
  {
    graphic_item_visible = resolve_visibility attribs;
    graphic_item_origin = coordinate_of sys_orig { x = cx; y = cy };
    graphic_item_rotation = -. degrees_of_radians rot;
    graphic_item_nature = nature
  }

and resolve_shape nature attribs =
  let unit =
    match nature with
    | Text _ -> "pt"
    | _ -> "mm" in
  
  let parse_color c rez = match rez with
    | None -> None
    | Some str -> Some (resolve_color c str)
  in
  let fill_color =
      parse_color gray92 (find_attribute "fill" attribs)
  and line_color =
      parse_color blue (find_attribute "stroke" attribs)
  in
  let (pattern, fill, line) =
      match (fill_color, line_color) with
      | (None,   None) -> (None, gray92,  blue)
      | (None, Some c) -> (None, gray92,  extract_primary_color c)
      | (Some (PatternColor (pat, fc, lc)), _) -> (Some pat, fc, lc)
      | (Some (SolidColor a), Some (SolidColor b)) -> (Some FillSolid, a, b)
      | (Some (SolidColor a), None) -> (Some FillSolid, a, blue)
      | (Some (SolidColor a), Some b) -> (Some FillSolid, a, extract_primary_color b)

  in
  {
    shape_line_color = line;
    shape_fill_color = fill;
    shape_pattern = Some LineSolid;
    shape_fill_pattern = pattern;
    shape_line_thickness = resolve_line_thickness unit attribs;
    shape_nature = nature
  }

and resolve_text_viewbox attribs align font_size =
  let w = 100.
  and h = font_size /. 0.6 in
  let x = resolve_float_attribute 0. "x" attribs
  and y =
    resolve_float_attribute (h /. 2. +. font_size *. 0.46) "y" attribs in
  match align with
  | Left -> x, y -. h /. 2. -. font_size *. 0.46, w, h
  | Center -> x -. w /. 2., y -. h /. 2. -. font_size *. 0.46, w, h
  | Right -> x -. w, y -. h /. 2. -. font_size *. 0.46, w, h

and resolve_text_alignment attribs =
  let s = resolve_string_attribute "" "text-anchor" attribs in
  resolve_text_anchor s

and points_viewbox points =
  let rec min_list l =
    match l with
    | [] -> max_float
    | f :: l -> min f (min_list l)
  and max_list l =
    match l with
    | [] -> -. max_float
    | f :: l -> max f (max_list l)
  and x_coord p = p.x
  and y_coord p = p.y in
  let min_x = min_list (List.map x_coord points)
  and min_y = min_list (List.map y_coord points)
  and max_x = max_list (List.map x_coord points)
  and max_y = max_list (List.map y_coord points) in
  min_x,
  min_y,
  max_x -. min_x,
  max_y -. min_y

and resolve_text_style attribs =
  let resolve_text_style' acc (name, value, text_style) =
    if resolve_string_attribute "" name attribs = value then
      text_style :: acc
    else acc in
  List.fold_left
    resolve_text_style'
    []
    [
      "font-style", "italic", Italic;
      "font-weight", "bold", Bold;
      "text-decoration", "underline", Underline
    ]

and resolve_font_family attribs =
  resolve_string_attribute "" "font-family" attribs

and resolve_visibility attribs =
  match resolve_string_attribute "" "visibility" attribs with
  | "hidden" -> false
  | _ -> true

and resolve_line_pattern attribs =
  let s = resolve_string_attribute "" "stroke-dasharray" attribs in
  resolve_stroke_dasharray s

and resolve_line_thickness unit attribs =
  let s = resolve_string_attribute "" "stroke-width" attribs in
  resolve_length unit 0.25 s

and resolve_points attribs =
  let rec resolve_points' fs =
    match fs with
    | [] | [_] -> []
    | f :: (f' :: fs) ->
        { x = f; y = f' } :: (resolve_points' fs) in
  let s = resolve_string_attribute "" "points" attribs in
  let points = Str.split (Str.regexp "[, \t\r]+") s in
  resolve_points' (List.map (resolve_float 0.) points)

and resolve_float_attribute default name attribs =
  match List.mem_assoc ("", name) attribs with
  | true -> resolve_float 0. (List.assoc ("", name) attribs)
  | false -> default

and resolve_text_anchor s =
  match s with
  | "end" -> Right
  | "middle" -> Center
  | _ -> Left

and resolve_stroke_dasharray s =
  match Str.split (Str.regexp "[, \t\r]+") s with
  | [ "5"; "5" ] -> Some LineDash
  | [ "1"; "2" ] -> Some LineDot
  | [ "5"; "2"; "1"; "2" ] -> Some LineDashDot
  | [ "5"; "2"; "1"; "2"; "1"; "2" ] -> Some LineDashDotDot
  | _ -> None

and resolve_length target_unit default s =
  let resolve_length' unit s =
    (float_of_string s) *. (unit_factor unit target_unit) in
  match String.length s with
  | 0 -> default
  | 1 | 2 -> resolve_length' "" s
  | l ->
      let unit = String.sub s (l - 2) 2
      and s' = String.sub s 0 (l - 2) in
      resolve_length' unit s'

and resolve_float default s =
  match s with
  | "" -> default
  | _ -> float_of_string s

and resolve_matrix s =
  match Str.split (Str.regexp "[, \t\r()]+") s with
  | [ "matrix"; a; b; c; d; e; f ] ->
      resolve_float 0. a,
      resolve_float 0. b,
      resolve_float 0. c,
      resolve_float 0. d,
      resolve_float 0. e,
      resolve_float 0. f
  | _ ->
      1., 0., 0., 1., 0., 0.

and in_tree i = 
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

