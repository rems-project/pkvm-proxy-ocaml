(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type nonrec 'a result = ('a, [`Msg of string]) result
let strf = Format.asprintf
let error_msg e = Error (`Msg e)

(* JSON values *)

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

let kind_of_json = function
| `Null -> "null"
| `Bool _ -> "bool"
| `Float _ -> "float"
| `String _ -> "string"
| `A _ -> "array"
| `O _ -> "object"

(* Queries *)

module Q = struct
  type json = t
  type path = [`A | `O of string] list (* in reverse order *)
  type 'a t = path -> json -> 'a

  (* Errors *)

  let path_to_string p =
    let seg = function `A -> "[]" | `O n -> "." ^ n in
    String.concat "" (List.rev_map seg p)

  let err p exp fnd =
    failwith (strf "JSON query path %s: expected: %s found: %s"
                (path_to_string p) exp (kind_of_json fnd))

  let err_miss_mem p =
    failwith (strf "JSON query path %s: no such member"
                (path_to_string p))

  (* Queries *)

  let json _ j = j
  let null p = function `Null -> () | j -> err p "null" j
  let nullable q p = function `Null -> None | j -> Some (q p j)
  let bool p = function `Bool b -> b | j -> err p "bool" j
  let int p = function `Float f -> truncate f (* XXX *) | j -> err p "int" j
  let float p = function `Float f -> f | j -> err p "float" j
  let string p = function `String s -> s | j -> err p "string" j
  let array qe p = function
  | `A es -> List.(rev @@ rev_map (qe (`A :: p)) es)
  | j -> err p "array" j

  let mem name qmem qobj p = function
  | `O ms as obj ->
      let pm = `O name :: p in
      begin match List.assoc name ms with
      | exception Not_found -> err_miss_mem pm
      | j -> qobj p obj (qmem pm j)
      end
  | j -> err p "object" j

  let mem_opt name qmem qobj p = function
  | `O ms as obj ->
      let pm = `O name :: p in
      begin match List.assoc name ms with
      | exception Not_found -> qobj p obj None
      | j -> qobj p obj (Some (qmem pm j))
      end
  | j -> err p "object" j

  let mems q p = function
    `O xs -> List.map (fun (k, v) -> (k, q (`O k :: p) v)) xs
  | j     -> err p "object" j

  let obj v p = function
  | `O _ -> v
  | j -> err p "object" j

  let get v = v
  let sel name qmem p = function (* optimize obj get |> mem name qmem *)
  | `O ms ->
      let pm = `O name :: p in
      begin match List.assoc name ms with
      | exception Not_found -> err_miss_mem pm
      | j -> qmem pm j
      end
  | j -> err p "object" j

  let map f q p j = q p j |> f

  let to_err q p j = match q p j with
  | Ok v -> v
  | Error (`Msg msg) -> err p msg j

  let map_r f q = map f q |> to_err

  let query q j = try Ok (q [] j) with Failure e -> error_msg e
end

(* JSON generation *)

module G = struct

  type json = t
  type enc = { mutable sep : bool; b : Buffer.t }
  type t = enc -> unit

  let addc c g = Buffer.add_char g.b c
  let adds s g = Buffer.add_string g.b s
  let adds_esc s g =
    let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false in
    let len = String.length s in
    let max_idx = len - 1 in
    let flush b start i =
      if start < len then Buffer.add_substring b s start (i - start);
    in
    let rec loop start i = match i > max_idx with
    | true -> flush g.b start i
    | false ->
        let next = i + 1 in
        match String.get s i with
        | '"' -> flush g.b start i; adds "\\\"" g; loop next next
        | '\\' -> flush g.b start i; adds "\\\\" g; loop next next
        | c when is_control c ->
            flush g.b start i; adds (strf "\\u%04X" (Char.code c)) g;
            loop next next
        | _ -> loop start next
    in
    loop 0 0

  type 'a seq = t
  let empty _ = ()
  let ( ++ ) s s' g = s g; s' g

  type mem
  type el

  let null g = adds "null" g
  let bool b g = match b with true -> adds "true" g | false -> adds "false" g
  let int i g = adds (string_of_int i) g
  (* let float f g = adds (string_of_float f) g *)
  let float f g = adds (strf "%.015g" f) g
  let string s g = addc '"' g; adds_esc s g; addc '"' g

  let nosep g = g.sep <- false
  let sep g = g.sep
  let set_sep sep g = g.sep <- sep
  let if_sep g = if not g.sep then g.sep <- true else addc ',' g

  let el e g = if_sep g; e g
  let el_if c v = if c then el (v ()) else empty
  let arr seq g = (* Not T.R. *)
    let sep = sep g in
    addc '[' g; nosep g; seq g; addc ']' g; set_sep sep g

  let mem m v g = if_sep g; string m g; addc ':' g; v g
  let mem_if c m v = if c then mem m (v ()) else empty
  let obj mems g = (* Not T.R. *)
    let sep = sep g in
    addc '{' g; nosep g; mems g; addc '}' g; set_sep sep g

  let rec of_json = function (* Not T.R. *)
  | `Null -> null
  | `Bool b -> bool b
  | `Float f -> float f
  | `String s -> string s
  | `A a ->
      let add_el acc e = acc ++ el (of_json e) in
      arr (List.fold_left add_el empty a)
  | `O o ->
      let add_mem acc (m, v) = acc ++ mem m (of_json v) in
      obj (List.fold_left add_mem empty o)

  (* Output generated values *)

  let buffer_add b g = g { sep = true; b }
  let kbuf k g = let b = Buffer.create 65525 in buffer_add b g; k b
  let to_string g = kbuf Buffer.contents g
  let output oc g = kbuf (fun b -> Buffer.output_buffer oc b) g
end

let for_string s = G.(to_string @@ string s)

(* Decode *)

type decoder = { t : Buffer.t; i : string; mutable pos : int; }

let decoder s = { t = Buffer.create 255; i = s; pos = 0 }
let accept d = d.pos <- d.pos + 1 [@@ ocaml.inline]
let treset d = Buffer.reset d.t [@@ ocaml.inline]
let taccept d = Buffer.add_char d.t d.i.[d.pos]; accept d; [@@ ocaml.inline]
let taddc d c = Buffer.add_char d.t c [@@ ocaml.inline]
let token d = Buffer.contents d.t [@@ ocaml.inline]
let eoi d = d.pos = String.length d.i [@@ ocaml.inline]
let byte d = match eoi d with
| true -> 0xFFF
| false -> Char.code d.i.[d.pos]
[@@ ocaml.inline]

let err d fmt =
  Format.kasprintf (fun s -> raise_notrace (Failure s)) ("%d: " ^^ fmt) d.pos

let pp_byte ppf d = match byte d with
| 0xFFF -> Format.fprintf ppf "end of input"
| b -> Format.fprintf ppf "%C" (Char.chr b)

type utf_8_case =
  | L1 | L2 | L3_E0 | L3_E1_EC_or_EE_EF | L3_ED | L4_F0 | L4_F1_F3 | L4_F4 | E

let utf_8_case =
(*
  (* See https://tools.ietf.org/html/rfc3629#section-4 *)
  Printf.printf "[|";
  for i = 0 to 255 do
    if i mod 16 = 0 then Printf.printf "\n";
    if 0x00 <= i && i <= 0x7F then Printf.printf "L1; " else
    if 0xC2 <= i && i <= 0xDF then Printf.printf "L2; " else
    if 0xE0 = i then Printf.printf "L3_E0; " else
    if 0xE1 <= i && i <= 0xEC || 0xEE <= i && i <= 0xEF
    then Printf.printf "L3_E1_EC_or_EE_EF; " else
    if 0xED = i then Printf.printf "L3_ED;" else
    if 0xF0 = i then Printf.printf "L4_F0; " else
    if 0xF1 <= i && i <= 0xF3 then Printf.printf "L4_F1_F3; " else
    if 0xF4 = i then Printf.printf "L4_F4; " else
    Printf.printf "E; "
  done;
  Printf.printf "\n|]"
*)
[|
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
  E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
  E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
  E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
  E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
  E; E; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
  L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
  L3_E0; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
  L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
  L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
  L3_E1_EC_or_EE_EF; L3_ED;L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
  L4_F0; L4_F1_F3; L4_F1_F3; L4_F1_F3; L4_F4; E; E; E; E; E; E; E; E; E; E; E;
|]

let taccept_utf_8 d =
  let err d = err d "expected UTF-8 byte found: %a" pp_byte d in
  let b = byte d in
  let accept_tail d =
    if (byte d lsr 6 = 0b10) then taccept d else err d [@@ ocaml.inline]
  in
  match utf_8_case.(b) with
  | L1 -> taccept d
  | L2 -> taccept d; accept_tail d
  | L3_E0 ->
      taccept d;
      if (byte d - 0xA0 < 0xBF - 0xA0) then taccept d else err d;
      accept_tail d
  | L3_E1_EC_or_EE_EF -> taccept d; accept_tail d; accept_tail d
  | L3_ED ->
      taccept d;
      if (byte d - 0x80 < 0x9F - 0x80) then taccept d else err d;
      accept_tail d
  | L4_F0 ->
      taccept d;
      if (byte d - 0x90 < 0x90 - 0xBF) then taccept d else err d;
      accept_tail d; accept_tail d
  | L4_F1_F3 -> taccept d; accept_tail d; accept_tail d; accept_tail d;
  | L4_F4 ->
      taccept d;
      if (byte d - 0x80 < 0x8F - 0x80) then taccept d else err d;
  | E -> err d

let accept_bytes d bytes = (* first byte already checked *)
  let max = String.length bytes - 1 in
  let rec loop i = match i > max with
  | true -> ()
  | false ->
      match Char.code bytes.[i] = byte d with
      | true -> accept d; loop (i + 1)
      | false ->
          err d "expected %C found: %a while parsing '%s'"
            bytes.[i] pp_byte d bytes
  in
  accept d; loop 1

let rec skip_ws d = match byte d with
| 0x20 | 0x09 | 0x0A | 0x0D -> accept d; skip_ws d
| _ -> ()

let parse_true d = accept_bytes d "true"; `Bool true
let parse_false d = accept_bytes d "false"; `Bool false
let parse_null d = accept_bytes d "null"; `Null
let parse_number d = (* not fully compliant *)
  let conv d = try `Float (float_of_string (token d)) with
  | Failure _ -> err d "could not parse a float from: %S" (token d)
  in
  let rec taccept_non_sep d = match byte d with
  | 0x20 | 0x09 | 0x0A | 0x0D | 0x2C | 0x5D | 0x7D | 0xFFF -> conv d
  | _ -> taccept d; taccept_non_sep d
  in
  treset d; taccept d; taccept_non_sep d

let parse_string d =
  let parse_escape d = match byte d with
  | (0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 as b) ->
      taddc d (Char.chr b); accept d;
  | 0x75 -> (* Unicode escapes are not treated. *)
      taddc d '\\'; taccept d;
  | _ -> err d "expected escape found: %a" pp_byte d
  in
  let rec loop d = match byte d with
  | 0x5C (* '\' *) -> accept d; parse_escape d; loop d
  | 0x22 (* '"' *) -> accept d; `String (token d)
  | 0xFFF -> err d "unclosed string"
  | _ -> taccept_utf_8 d; loop d
  in
  accept d; treset d; loop d

let rec parse_object d = match (accept d; skip_ws d; byte d) with
| 0x7D (* '}' *) -> accept d; `O []
| _ ->
    let parse_name d =
      let `String name = match (skip_ws d; byte d) with
      | 0x22 (* '"' *) -> parse_string d
      | _ -> err d "expected '\"' found: %a" pp_byte d
      in
      skip_ws d; name
    in
    let rec loop acc d =
      let name = parse_name d in
      match byte d with
      | 0x3A (* ':' *) ->
          let v = (accept d; parse_value d) in
          begin match byte d with
          | 0x2C (* ',' *) -> accept d; loop ((name, v) :: acc) d
          | 0x7D (* '}' *) -> accept d; `O (List.rev ((name, v) :: acc))
          | _ -> err d "expected ',' or '}' found: %a" pp_byte d
          end
      | _ -> err d "expected ':' found: %a" pp_byte d
    in
    loop [] d

and parse_array d = match (accept d; skip_ws d; byte d) with
| 0x5D (* ']' *) -> accept d; `A []
| _ ->
    let rec loop acc d =
      let v = parse_value d in
      match byte d with
      | 0x2C (* ',' *) -> accept d; loop (v :: acc) d
      | 0x5D (* ']' *) -> accept d; `A (List.rev (v :: acc))
      | _ -> err d "expected ',' or ']' found: %a" pp_byte d
    in
    loop [] d

and parse_value d : t =
  let v = match (skip_ws d; byte d) with
  | 0x22 (* '"' *) -> parse_string d
  | 0x74 (* 't' *) -> parse_true d
  | 0x66 (* 'f' *) -> parse_false d
  | 0x6E (* 'n' *) -> parse_null d
  | 0x7B (* '{' *) -> parse_object d
  | 0x5B (* '[' *) -> parse_array d
  | 0x2D (* '-' *) -> parse_number d
  | b when 0x30 (* '0' *) <= b && b <= 0x39 (* '9' *) -> parse_number d
  | _ -> err d "expected a JSON value found: %a" pp_byte d
  in
  skip_ws d;
  v

let of_string_prefix s =
  try
    let d = decoder s in
    let v = parse_value d in
    let s = String.(sub s d.pos (length s - d.pos)) in
    Ok (v, s)
  with Failure e -> error_msg e

let of_string s =
  try
    let d = decoder s in
    let v = parse_value d in
    match byte d with
    | 0xFFF (* eoi *) -> Ok v
    | _ -> err d "expected end of input found: %a" pp_byte d
  with
  | Failure e -> error_msg e

let to_string v = G.(to_string @@ of_json v)

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
