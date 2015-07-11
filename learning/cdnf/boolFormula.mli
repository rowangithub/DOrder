(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************)

type var_t = int
type lit_t = int

type t = Lit of lit_t | Not of t | And of t array | Or of t array

val is_positive : lit_t -> bool

val of_var : lit_t -> var_t

val from_dimacs : in_channel -> int * t

val from_boolformula_t : Cdnfp.boolformula_t -> t

val print : t -> unit

