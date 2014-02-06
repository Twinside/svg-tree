
%{

type coord = float

type point = coord * coord

type origin = Absolute | Relative

type svg_path
    = MoveTo of (origin * point list)
    | LineTo of (origin * point list)

    | HorizontalTo of (origin * coord list)
    | VerticalTo of (origin * coord list)

    | CurveTo of (origin * (point * point * point) list)
    | SmoothCurveTo of (origin * (point * point) list)
    | QuadraticBezier of (origin * (point * point) list)
    | SmoothQuadraticBezierCurveTo of (origin * point list)
    | ElipticalArc of (origin * (coord * coord * coord * coord * coord * point) list)
    | EndPath

%}

%token EOF COMMA PLUS MINUS
%token Q M L H V C S T A Z
%token Qrel Mrel Lrel Hrel Vrel Crel Srel Trel Arel
%token<float> NUM

%type <svg_path list> svg_path
%start svg_path

%%
svg_path
    : moveto_drawto_command_groups { $1 }
    ;

moveto_drawto_command_groups
    : moveto_drawto_command_group
        { $1 }
    | moveto_drawto_command_groups moveto_drawto_command_group 
        { $1 @ $2 }
    ;

moveto_drawto_command_group
    : moveto
        { [$1] }
    | moveto drawto_commands
        { $1 :: List.rev $2 }
    ;

drawto_commands
    : drawto_command
        { [$1] }
    | drawto_commands drawto_command 
        { $2 :: $1 }
    ;

drawto_command
    : Z { EndPath }
    | L coordinate_pair_list
        { LineTo (Absolute, List.rev $2) }
    | Lrel coordinate_pair_list
        { LineTo (Relative, List.rev $2) }

    | H coordinate_list
        { HorizontalTo (Absolute, List.rev $2)  }
    | Hrel coordinate_list
        { HorizontalTo (Relative, List.rev $2)  }

    | V coordinate_list
        { VerticalTo (Absolute, List.rev $2) }
    | Vrel coordinate_list
        { VerticalTo (Relative, List.rev $2) }

    | C curveto_argument_sequence
        { CurveTo (Absolute, List.rev $2) }
    | Crel curveto_argument_sequence
        { CurveTo (Relative, List.rev $2) }


    | S point_pair_list
        { SmoothCurveTo (Absolute, List.rev $2) }
    | Srel point_pair_list
        { SmoothCurveTo (Relative, List.rev $2) }

    | Q point_pair_list
        { QuadraticBezier (Absolute, List.rev $2) }
    | Qrel point_pair_list
        { QuadraticBezier (Relative, List.rev $2) }

    | T coordinate_pair_list
        { SmoothQuadraticBezierCurveTo (Absolute, List.rev $2) }
    | Trel coordinate_pair_list
        { SmoothQuadraticBezierCurveTo (Relative, List.rev $2) }

    | A elliptical_arc_argument_sequence
        { ElipticalArc (Absolute, List.rev $2) }
    | Arel elliptical_arc_argument_sequence
        { ElipticalArc (Relative, List.rev $2) }
    ;

moveto
    : M coordinate_pair_list
        { MoveTo (Absolute, List.rev $2) }
    | Mrel coordinate_pair_list
        { MoveTo (Relative, List.rev $2) }
    ;

comma_wsp
    : { () }
    | COMMA { () }
    ;

coordinate_pair_list
    : coordinate_pair { [$1] }
    | coordinate_pair_list comma_wsp coordinate_pair  { $3 :: $1 }
    ;

coordinate_list
    : coordinate    { [$1] }
    | coordinate_list comma_wsp coordinate
                    { $3 :: $1 }
    ;


curveto_argument_sequence
    : curveto_argument { [$1] }
    | curveto_argument_sequence comma_wsp curveto_argument
        { $3 :: $1 }
    ;

curveto_argument
    : coordinate_pair comma_wsp coordinate_pair comma_wsp coordinate_pair
        { ($1, $3, $5) }
    ;

point_pair_list
    : point_pair { [ $1 ] }
    | point_pair_list comma_wsp point_pair
        { $3 :: $1 }
    ;

point_pair
    : coordinate_pair comma_wsp coordinate_pair
        { ($1, $3) }
    ;

elliptical_arc_argument_sequence
    : elliptical_arc_argument
        { [$1] }
    | elliptical_arc_argument_sequence comma_wsp elliptical_arc_argument
        { $3 :: $1 }
    ;

elliptical_arc_argument
    : NUM    comma_wsp
      NUM    comma_wsp 
      number comma_wsp
      NUM    comma_wsp
      NUM    comma_wsp
      coordinate_pair
        { ($1, $3, $5, $7, $9, $11) }
    ;

coordinate_pair
    : coordinate COMMA coordinate { ($1, $3) }
    | coordinate coordinate       { ($1, $2) }
    ;

coordinate: number { $1 };

number
    : NUM { $1 }
    | PLUS NUM { $2 }
    | MINUS NUM { (-. ($2)) }
    ;

%%

