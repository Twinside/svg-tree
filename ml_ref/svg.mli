(*
 *  Modelica compiler API
 *
 *  Copyright (C) 2005 - 2007 Imagine S.A.
 *                2007 - 2010 LMS Imagine
 *  For more information or commercial use please contact us at www.lmsintl.com
 *
 *)

open Annotation

type transform =
  | Rotation of float * float * float
  | Translation of float * float
  | Scale of float * float
  | Matrix of matrix

and matrix = float * float * float * float * float * float

val placement_point:
  Annotation.layer -> Annotation.transformation option -> point

val placement_transformation:
  Annotation.layer -> float -> Graphics.t ->
  Annotation.transformation option -> matrix

val to_string:
  Scoping.context Lazy.t -> String.t -> Graphics.t -> float -> bool ->
  String.t

val to_transformation:
  Annotation.layer -> Annotation.layer -> matrix ->
  Annotation.transformation

(** [to_layer svg] Conversion from svg string to Modelica layer *)
val to_layer: String.t -> Annotation.layer

(** [resolve_graphics layer svg] Conversion of an svg graphic to Modelica using a given coordinate system *)
val resolve_graphics:
  Annotation.coordinate_system -> String.t -> Annotation.graphic_item list

