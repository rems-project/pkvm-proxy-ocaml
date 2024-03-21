(** Parallelism prims *)

type 'a thread
(** Joinable parallel things. *)

val spawn : cpu:int -> (unit -> 'a) -> 'a thread
(** [spawn ~core f] runs [f] in parallel with the current thread, on the
    physical core [core]. *)

val join : 'a thread -> 'a
val spawnv : cpus:int -> (int -> 'a) -> 'a list
val cores : int

(* Test entrypoints *)

type test
val test : ?desc:string -> string -> (unit -> unit) -> test
val main : test list -> unit
