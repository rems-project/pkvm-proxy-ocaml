external sched_setaffinity : thread:int -> int array -> unit = "caml_sched_setaffinity"
external sched_getaffinity : thread:int -> int array = "caml_sched_getaffinity"

module Log = (val Logs.(Src.create "pkvm_tests" |> src_log))

(** Parallelism. **)

(*
 * Mk.1: directly reuse domains. Domains correspond to kernel threads.
 *)

type 'a thread = 'a Domain.t

(* While glibc discovers the number of processors parsing /sys, musl
   (sysconf(_SC_NPROCESSORS_ONLN)) and OCaml rt
   (Domain.recommended_domain_count) just take the size of the affinity mask. *)
let initial_affinity_mask = sched_getaffinity ~thread:0
let cores = Array.length initial_affinity_mask

(* Including the module will silently pin the main thread to core 0. *)
let _ = sched_setaffinity ~thread:0 [|0|]

(* Why does map_region_guest use vcpu_memcache? Otherwise it would be
   independent of vcpu. *)

let spawn ~cpu f = Domain.spawn @@ fun () ->
  ( try sched_setaffinity ~thread:0 [|cpu|]
    with Unix.Unix_error(Unix.EINVAL, _, _) ->
      Log.err (fun k -> k "Cannot pin thread to CPU %d — are we running with enough cpus?" cpu);
      Fmt.invalid_arg "spawn: cannot use cpu %d" cpu );
  f()
let join = Domain.join

let spawnv ~cpus f =
  List.init cpus (fun cpu -> spawn ~cpu (fun () -> f cpu)) |> List.map join


(** Config. **)

module Smap = Map.Make(String)

let disable xs =
  let m = Smap.of_seq (List.to_seq xs) in
  fun x -> Smap.find_opt x m |> Option.value ~default:true

let schema =
  let open Json.Q in
  obj (fun ll ts ->
    Option.join ll,
    Option.fold ~none:(fun _ -> true) ~some:disable ts)
  |> (mem_opt "loglevel" (string |> map_r Logs.level_of_string))
  |> mem_opt "run" (mems bool)

let buf_add_fd buf fd =
  let bs = Bytes.create 4096 in
  let rec go () = match Unix.read fd bs 0 4096 with
  | 0 -> ()
  | n -> Buffer.add_subbytes buf bs 0 n; go () in
  go ()

let ( let* ) = Result.bind

let to_unix_result f x = match f x with
| exception Unix.Unix_error (err, _, _) -> Error (`Msg (Unix.error_message err))
| v -> Ok v

let cfg = Fmt.str "%s.json" Sys.argv.(0)

let load_cfg ?(file = cfg) () =
  let buf = Buffer.create 17 in
  let* fd = to_unix_result Unix.(openfile file [O_RDONLY]) 0 in
  buf_add_fd buf fd; Unix.close fd;
  let* json = Json.of_string (Buffer.contents buf) in
  let* level, run = Json.Q.query schema json in
  Ok (level, run)

(** Entry points **)

let pp_tag = Fmt.(styled `Bold @@ styled `Green @@ styled `Italic string)

let main xs =
  Logs_threaded.enable ();

  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Warning);

  let select = match load_cfg () with
  | Ok (ll, f) ->
      Logs.set_level ~all:true ll;
      f
  | Error (`Msg msg) ->
      Log.warn (fun k -> k "`%s': %s" cfg msg);
      fun _ -> true
  in
  xs |> List.iteri (fun i (name, test) ->
    match select name with
    | true ->
        Log.app (fun k -> k "@.╭────────────────────@.│ start (#%d): %a@." i pp_tag name);
        test ();
        Log.app (fun k -> k "@.│ ok: %a@.╰────────────────────@." pp_tag name);

    | false ->
        Log.app (fun k -> k "@.== skip: %a@." pp_tag name)
  );
  Log.app (fun k -> k "all done")
