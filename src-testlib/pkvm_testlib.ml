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
  ( try sched_setaffinity ~thread:0 [|cpu|] with
    Unix.Unix_error(Unix.EINVAL, _, _) ->
      Log.err (fun k -> k "Cannot pin thread to CPU %d — are we running with enough cpus?" cpu);
      Fmt.invalid_arg "spawn: cannot use cpu %d" cpu );
  f()
let join = Domain.join

let spawnv ~cpus f =
  List.init cpus (fun cpu -> spawn ~cpu (fun () -> f cpu)) |> List.map join

(** Test assertions *)

exception Assert
exception Expected_failure

let pkvm_assert = function false -> raise Assert | _ -> ()
let pkvm_expect_error f x = match f x with
| exception (Pkvm_proxy.HVC _) -> ()
| _ -> raise Expected_failure

(** Step VCPU, with expectations. *)

module Cond = struct
  open Pkvm_proxy
  type t =
  | No_regs of (int -> fault_info -> bool)
  | Regs of (int -> fault_info -> registers -> bool)
  let lift2 (++) f g = match f, g with
  | No_regs f, No_regs g -> No_regs (fun a b -> f a b ++ g a b)
  | Regs f, Regs g -> Regs (fun a b c -> f a b c ++ g a b c)
  | Regs f, No_regs g | No_regs g, Regs f -> Regs (fun a b c -> f a b c ++ g a b)
  let (&&&) = lift2 (&&) and (|||) = lift2 (||)
  let exit f = No_regs (fun ex _ -> f ex)
  let exit_is e = exit (fun x -> x = e)
  let fault f = No_regs (fun _ -> f)
  let regs f = Regs (fun _ _ -> f)
end

type vcpu_cond = Cond.t

let rec vcpu_run_expect ?cond vcpu =
  let open Pkvm_proxy in
  match vcpu_run vcpu with
  | 0 -> vcpu_run_expect ?cond vcpu
  | exit ->
      match cond with
      | None -> ()
      | Some (Cond.No_regs f) ->
          let flt = vcpu.mem.@[vcpu_fault] in
          if not (f exit flt) then (
            Log.err (fun k -> k "@[vcpu_run:@ exit %d@ fault %a@]" exit pp_fault_info flt);
            raise Assert)
      | Some (Cond.Regs f) ->
          vcpu_sync_state ();
          let flt = vcpu.mem.@[vcpu_fault] and regs = vcpu.mem.@[vcpu_regs] in
          if not (f exit flt regs) then (
            Log.err (fun k -> k "@[vcpu_run@ exit %d@ fault %a@ regs %a@]" exit pp_fault_info flt pp_regs regs);
            raise Assert)

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

let rec p_root p =
  match String.rindex_opt p '.' with
  | None -> p
  | Some i0 ->
      match String.rindex_opt p '/' with
      | Some i1 when i1 > i0 -> p
      | _ -> p_root (String.sub p 0 i0)

let cmdname = p_root Sys.argv.(0)

let cfg = Fmt.str "%s.json" cmdname

let load_cfg ?(file = cfg) () =
  let buf = Buffer.create 17 in
  let* fd = to_unix_result Unix.(openfile file [O_RDONLY]) 0 in
  buf_add_fd buf fd; Unix.close fd;
  let* json = Json.of_string (Buffer.contents buf) in
  let* level, run = Json.Q.query schema json in
  Ok (level, run)

(** Entry points **)

type test = { f : unit -> unit; name : string; desc : string; }

let test ?(desc = "<NO DESC>") name f = { name; desc; f = fun x -> ignore (f x) }

let pp_test_ok = Fmt.(styled `Green @@ styled `Bold @@ styled `Italic string)
let pp_test_err = Fmt.(styled `Red @@ styled `Bold @@ styled `Italic string)

let pp_t_start ppf (i, t) =
  Fmt.pf ppf "@.╭────────────────────@.│ start (#%d): %a@." i pp_test_ok t.name
let pp_t_end ppf t =
  Fmt.pf ppf "@.│ ok: %a@.╰────────────────────@." pp_test_ok t.name
let pp_t_error pp_err ppf (t, e) =
  Fmt.pf ppf "@.│ error: %a: %a@.╰────────────────────@." pp_test_err t.name pp_err e
let pp_exn ppf exn = Fmt.string ppf (Printexc.to_string exn)

let kcov_file = "/output/kcov-addr"

let with_kcov_ppf f =
  let oc = open_out kcov_file in
  let ppf = Format.formatter_of_out_channel oc in
  let res = f ppf in
  Format.pp_print_flush ppf ();
  close_out oc;
  res

let run1 ?(select = fun _ -> true) ~index ?kcov t =
  match select t.name with
  | false -> Log.app (fun k -> k "@.== skip: %a@." pp_test_ok t.name)
  | true -> 
      Log.app (fun k -> k "%a" pp_t_start (index, t));
      kcov |> Option.iter Pkvm_kcov.enable;
      ( try t.f () with exn ->
          Log.app (fun k -> k "%a" (pp_t_error pp_exn) (t, exn));
          exit 1 );
      kcov |> Option.iter (fun k ->
        Pkvm_kcov.disable k;
        with_kcov_ppf (fun ppf -> Fmt.pf ppf "%a@." Pkvm_kcov.pp k));
      Log.app (fun k -> k "%a" pp_t_end t)

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
  let kcov = Pkvm_kcov.create ~size:51200 ()
  in
  xs |> List.iteri (fun i test -> run1 ~select ~index:i ?kcov test);
  Log.app (fun k -> k "all done")
