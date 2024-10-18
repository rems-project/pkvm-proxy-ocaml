(** Parallelism prims **)

type 'a thread
(** Joinable parallel things. *)

val spawn : cpu:int -> (unit -> 'a) -> 'a thread
(** [spawn ~core f] runs [f] in parallel with the current thread, on the
    physical core [core]. *)

val join : 'a thread -> 'a
val spawnv : cpus:int -> (int -> 'a) -> 'a list
val cores : int

(** Test assertions *)

val pkvm_assert : bool -> unit
val pkvm_expect_proxy_error : ('a -> 'b) -> 'a -> unit
val pkvm_expect_error : ('a -> 'b) -> 'a -> unit

(** Conditionally step VCPUs. **)

type vcpu_cond

module Cond : sig
  open Pkvm_proxy
  val exit : (int -> bool) -> vcpu_cond
  val exit_is : int -> vcpu_cond
  val fault : (fault_info -> bool) -> vcpu_cond
  val regs : (registers -> bool) -> vcpu_cond
  val (&&&) : vcpu_cond -> vcpu_cond -> vcpu_cond
  val (|||) : vcpu_cond -> vcpu_cond -> vcpu_cond
end

val vcpu_run_expect : ?cond:vcpu_cond -> Pkvm_proxy.vcpu -> unit
(** Steps {{!vcpu_run}} until it returns something other than 0.

    Check the optional expected condition [cond]. Raises on failure. *)

(** Test entrypoints **)

type test
val test : ?desc:string -> string -> (unit -> 'a) -> test
val main : ?want_cpus:int -> test list -> unit
