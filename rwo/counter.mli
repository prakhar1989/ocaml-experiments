open Core.Std

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts *)
val empty : t

(** Bump the frequency count for the given string *)
val touch : t -> string -> t

(** Converts the set of frequency counts to an association list. 
 A string shows up at most once, and the counts are >= 1 *)
val to_list : t -> (string * int) list

(** Representing a median - string vs tuples for odd / even collections *)
type median = | Median of string
              | Before_and_after of string * string

val median : t -> median 
