open Bigarray

module Bigstring = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  (* These are of the native-endian variety. *)
  external get_int64 : t -> int -> int64 = "%caml_bigstring_get64"
  external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64"
  external get_int32 : t -> int -> int32 = "%caml_bigstring_get32"
  external set_int32 : t -> int -> int32 -> unit = "%caml_bigstring_set32"
  let set_uint8 t i v = t.{i} <- Char.unsafe_chr (v land 0xff)
  let get_uint8 t i = Char.code t.{i}
  let set_bool t i v = t.{i} <- (if v then '\001' else '\000')
  let get_bool t i = t.{i} <> '\000'
  let blit_from_string ?(dst_i = 0) ?(src_i = 0) ?n src dst =
    (* XXX reuse any of the memcp-based fast blits, after juggling the types. *)
    let n = match n with Some n -> n | _ -> String.length src - src_i in
    if Array1.dim dst - dst_i < n || String.length src - src_i < n then
      invalid_arg "Bigstring.blit_string: too many bytes to blit.";
    for i = 0 to n - 1 do
      dst.{dst_i + i} <- src.[src_i + i]
    done
  let hex ?w () ppf s = Fmt.hex ?w () ppf (Array1.dim s, Array1.get s)
end

module Int64 = struct
  include Int64
  let (+) = add
  let (asr) = shift_right
  let (lsr) = shift_right_logical
  let (lsl) = shift_left
end

external _sched_setaffinity : int -> int array -> unit = "caml_sched_setaffinity"
let sched_setaffinity ?(thread = 0) = _sched_setaffinity thread

