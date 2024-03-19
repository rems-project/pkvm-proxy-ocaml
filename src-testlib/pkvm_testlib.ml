module Log = (val Logs.(Src.create "pkvm_testlib" |> src_log))

external sched_setaffinity : thread:int -> int array -> unit = "caml_sched_setaffinity"
external sched_getaffinity : thread:int -> int array = "caml_sched_getaffinity"

(* Parallelism Mk.1: directly reuse domains. Domains correspond to kernel
   threads. *)

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

