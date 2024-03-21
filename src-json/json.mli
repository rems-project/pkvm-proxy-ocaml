(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** JSON text codec and queries.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)

(** {1 Generic JSON representation} *)

type nonrec 'a result = ('a, [`Msg of string]) result

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]
(** The type for generic JSON text representations. *)

val of_string : string -> t result
(** [of_string s] parses JSON text from [s] according to
    {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
    limitations:
    {ul
    {- Numbers are parsed with [string_of_float] which is not
       compliant.}.
    {- Unicode escapes are left unparsed (this will not round trip
       with {!to_string}).}} *)

val of_string_prefix : string -> (t * string) result

val to_string : t -> string
(** [to_string v] is [v] as JSON text, encoded according to
    {{:https://tools.ietf.org/html/rfc8259}RFC8259} *)

val for_string : string -> string
(** [for_string s] is [to_string (`String s)]. *)

(** JSON value queries. *)
module Q : sig

  (** {1:query Queries} *)

  type json = t
  (** The type generic json representations. *)

  type 'a t
  (** The type for a query on a JSON value returning values of type ['a]. *)

  val null : unit t
  (** [null] queries a null JSON value. *)

  val nullable : 'a t -> 'a option t
  (** [nullable q] queries either a null JSON value or with [q]. *)

  val bool : bool t
  (** [bool] queries a boolean JSON value. *)

  val int : int t
  (** [int] queries an float JSON value and {!truncate}'s it. *)

  val float : float t
  (** [float] queries a float JSON value. *)

  val string : string t
  (** [string] queries a string JSON value. *)

  val array : 'a t -> 'a list t
  (** [array q] queries the elements of a JSON array with [q]. *)

  val mem : string -> 'a t -> ('a -> 'b) t -> 'b t
  (** [mem name q o] queries a JSON object [o]'s member [name] with [q]. *)

  val mem_opt : string -> 'a t -> ('a option -> 'b) t -> 'b t
  (** [mem_opt name q] queries a JSON object [o]'s optional member [name]
      with [q]. *)

  val mems : 'a t -> (string * 'a) list t

  val obj : 'a -> 'a t
  (** [obj v] queries an object and returns [v]. *)

  val get : 'a -> 'a
  (** [get] is the identity function *)

  val sel : string -> 'a t -> 'a t
  (** [sel name q] is [obj get |> mem name q] *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val to_err : 'a result t -> 'a t

  val map_r : ('a -> 'b result) -> 'a t -> 'b t

  val json : json t
  (** [json] queries any JSON value. *)

  val query : 'a t -> json -> 'a result
  (** [query q j] queries a JSON value [j] with [q]. *)
end

(** JSON value generation. *)
module G : sig

  (** {1:gen Generation} *)

  type json = t
  (** The type generic json representations. *)

  type 'a seq
  (** The type for sequences. *)

  val empty : 'a seq
  (** An empty sequence. *)

  val ( ++ ) : 'a seq -> 'a seq -> 'a seq
  (** [s ++ s'] is sequence [s'] concatenated to [s]. *)

  type t
  (** The type for generated JSON values. *)

  type mem
  (** The type for generated JSON members. *)

  type el
  (** The type for generated JSON array elements. *)

  val null : t
  (** [null] is the generated JSON null value. *)

  val bool : bool -> t
  (** [bool b] is [b] as a generated JSON boolean value. *)

  val int : int -> t
  (** [int i] is [i] as a generated JSON number. *)

  val float : float -> t
  (** [float f] is [f] as a generated JSON number. *)

  val string : string -> t
  (** [str s] is [s] as a generated JSON string value. *)

  val el : t -> el seq
  (** [el v] is [v] as a generated JSON array element. *)

  val el_if : bool -> (unit -> t) -> el seq
  (** [el_if c v] is [el (v ())] if [c] is [true] and {!empty} otherwise. *)

  val arr : el seq -> t
  (** [arr els] is a generated array whose values are generated by the
      elements [els]. *)

  val mem : string -> t -> mem seq
  (** [mem name v] is a generated object member whose name is [name] and
      value is [v]. *)

  val mem_if : bool -> string -> (unit -> t) -> mem seq
  (** [mem_if c name v] is [mem name v] if [c] is [true] and {!empty}
      otherwise. *)

  val obj : mem seq -> t
  (** [obj mems] is a generated object whose members are generated by [mems]. *)

  val of_json : json -> t
  (** [of_json v] is the JSON value [v] as a generated value. *)

  (** {1:output Output} *)

  val to_string : t -> string
  (** [to_string g] is the generated JSON value [g] as a string. *)

  val output : out_channel -> t -> unit
  (** [output oc g] outputs the generated JSON value [g] on [oc]. *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated JSON value [g] to [b]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
