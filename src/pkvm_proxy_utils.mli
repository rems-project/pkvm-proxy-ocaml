(** Random things that could (should?) have been anywhere else. *)

(** Bigarray = array allocated off the managed heap
    Bigstring = Bigarray interpreted as a char array

    We use these to represent someone else's memory. This module exposes a few
    relevant typed accessors and utils. *)
module Bigstring : sig
  open Bigarray
  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  (* These are of the native-endian variety. *)
  val get_int64 : t -> int -> int64
  val set_int64 : t -> int -> int64 -> unit
  val get_int32 : t -> int -> int32
  val set_int32 : t -> int -> int32 -> unit
  val set_uint8 : t -> int -> int -> unit
  val get_uint8 : t -> int -> int
  val set_bool  : t -> int -> bool -> unit
  val get_bool  : t -> int -> bool
  val blit_from_string : ?dst_i:int -> ?src_i:int -> ?n:int -> string -> t -> unit
  val hex : ?w:int -> unit -> t Fmt.t
end

(** Because having standard naming for standard operations was just too much. *)
module Int64 : sig
  include module type of Int64
  val (+) : int64 -> int64 -> int64
  val (asr) : int64 -> int -> int64
  val (lsr) : int64 -> int -> int64
  val (lsl) : int64 -> int -> int64
end

val sched_setaffinity : ?thread:int -> int array -> unit
