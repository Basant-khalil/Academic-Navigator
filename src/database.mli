(** Representation of static class roster database data. This module
    contains types which are used to represent a course and its various
    properties. The functions in this module are used to determine
    information from this data. *)

(** The type representing the categories of time *)
type timings =
  | Early_Morning
  | Morning
  | Lunch
  | Afternoon
  | Evening

type instructor
(** The abstract type of values representing an instructor *)

type meeting
(** The abstract type of values representing meeting information *)

type section
(** The abstract type of values representing sections *)

type t
(** The abstract type of values representing class information *)

type enrollGroup
(** The abstract type of value representing information about each
    section in a class *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the course that [j] represents. Requires: [j] is a
    valid JSON course representation.*)

val enrolled_group : int -> t -> enrollGroup
(** [enrolled_group n t] is the nth enrollment group among the list of
    enrollment groups included in [t]. *)

val group_section : enrollGroup -> section
(** [group_section g] gives the section which is nested in the
    enrollGroup [g] *)

val from_json_roster : string -> bool
(** [from_json_roster j] is true if j is one of the courses in
    roster.json. j is a name of a course class info that [j] represents.*)

val find_prereq : t -> string
(** [find_prereq a] is the identifier of the pre reqs in class
    information [a]. *)

val find_credits : enrollGroup -> int
(** [find_credits a] is the identifier of the credits in class
    information [a]. *)

val find_forbidden_overlap : t -> string
(** [find_forbbidden_overlap a] is the identifier of the forbidden
    overlaps in class information [a]. *)

val find_location : section -> string
(** [find_location a] is the identifier of the locations in class
    information [a]. *)

val find_description : t -> string
(** [find_description a] is the identifier of the descriptions in class
    information [a]. *)

val conv_time : string -> timings
(** [conv_time t] convert the [t] to timings. *)

val time_sort : string -> string -> string list
(** [time_sort s t] sorts the [t] for a given subject [s]. *)

val credits_from_roster : string -> int
(** [credits_from_roster] finds the credits of a course by the course
    name. *)

val sum_of_scores : (string * float) list -> float
(** [sum_of_scores lst] calculate the total score given by the [lst]. *)

val sum_of_credits : (string * float) list -> float
(** [sum_of_credits lst] calculate the total credits given by the [lst]. *)

val calculate_gpa : (string * float) list -> float
(** [calculate_gpa lst] calculates the weighted scores given by the
    [lst]. *)
