open Bigarray

external kcov_init : Unix.file_descr -> int -> unit = "caml_kcov_init"
external kcov_enable : Unix.file_descr -> unit = "caml_kcov_enable"
external kcov_disable : Unix.file_descr -> unit = "caml_kcov_disable"

module Log = (val Logs.(Src.create "Pkvm_proxy" |> src_log))

type kcov = Unix.file_descr * (int64, int64_elt, c_layout) Array1.t

let create ?(size = 1024) () =
  match Unix.(openfile "/sys/kernel/debug/kcov" [O_RDWR] 0) with
  | exception Unix.Unix_error _ ->
      Log.warn (fun k -> k "Cannot open KCOV — is it disabled?");
      None
  | fd ->
      kcov_init fd size;
      let buf =
        Unix.map_file fd Bigarray.int64 Bigarray.c_layout true [|size|]
          |> Bigarray.array1_of_genarray in
      Some (fd, buf)

let close (fd, _) = Unix.close fd

let enable ?(append = false) (fd, buf) =
  if not append then buf.{0} <- 0L; kcov_enable fd

let disable (fd, _) = kcov_disable fd

module IMap = Map.Make(Int64)

let get (_, buf) =
  let n = Int64.to_int buf.{0} in
  if n = Array1.dim buf - 1 then Log.warn (fun k -> k "KCOV: overflow, insufficient cover size");
  let map = ref IMap.empty in
  for i = 1 to n do
    map := IMap.update buf.{i} (function Some k -> Some (k + 1) | _ -> Some 1) !map;
  done;
  IMap.to_seq !map

let pp ppf kcov =
  Fmt.pf ppf "@[<v>%a@]" Fmt.(seq ~sep:cut (fmt "0x%Lx"))
    (get kcov |> Seq.map fst)
