type kcov

val create : ?size:int -> unit -> kcov option
val close : kcov -> unit
val enable : ?append:bool -> kcov -> unit
val disable : kcov -> unit
val get : kcov -> (int64 * int) Seq.t
val pp : kcov Fmt.t
