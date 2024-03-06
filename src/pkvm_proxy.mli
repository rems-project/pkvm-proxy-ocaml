open Pkvm_proxy_utils

(** {1 Hypercalls} *)

(** Host hypercalls.

    Mirros [enum __kvm_host_smccc_func], exposed by Hyp-proxy, and their
    respective ioctl arguments. **)
type _ host_smccc_func =

(* Hypercalls available only prior to pKVM finalisation *)
  | Kvm_hyp_init
  | Kvm_get_mdcr_el2
  | Pkvm_init
  | Pkvm_create_private_mapping
  | Pkvm_cpu_set_vector
  | Kvm_enable_ssbs
  | Vgic_v3_init_lrs
  | Vgic_v3_get_gic_config
  | Kvm_flush_vm_context
  | Kvm_tlb_flush_vmid_ipa
  | Kvm_tlb_flush_vmid
  | Kvm_flush_cpu_context
  | Pkvm_prot_finalize

(* Hypercalls available after pKVM finalisation *)
  | Pkvm_host_share_hyp    : int64 -> unit host_smccc_func (* highly unsafe *)
  | Pkvm_host_unshare_hyp  : int64 -> unit host_smccc_func
  | Pkvm_host_reclaim_page : int64 -> unit host_smccc_func
  | Pkvm_host_map_guest    : int64 * int64 -> unit host_smccc_func
  | Kvm_adjust_pc
  | Kvm_vcpu_run           : int64 -> int host_smccc_func
  | Kvm_timer_set_cntvoff
  | Vgic_v3_save_vmcr_aprs
  | Vgic_v3_restore_vmcr_aprs
  | Pkvm_init_vm           : int64 * int64 * int64 * int64 -> int host_smccc_func
  | Pkvm_init_vcpu         : int * int64 * int64 -> unit host_smccc_func
  | Pkvm_teardown_vm       : int -> unit host_smccc_func
  | Pkvm_vcpu_load         : int * int * int64 -> unit host_smccc_func
  | Pkvm_vcpu_put          : unit host_smccc_func
  | Pkvm_vcpu_sync_state   : unit host_smccc_func

val pp_host_smccc_func : 'a host_smccc_func Fmt.t

val hvc : 'a host_smccc_func -> 'a
(** [hvc func] performs the (host) hypercall described by [func]. *)

(** {1 Memory regions} *)

type 'a region
(** A kernel memory region.

    It can be obtained and given to something else, or shared. In the latter
    case, it can be read/written to.

    The type parameter is phantom and reflects the structured data in the region
    (if any). The parameter is inferred from record-field accesses. **)

(*  Region is tied to and FD. Closing the FD, including when the process dies,
    frees the region.

    Release removes this link. A region marked as released won't be auto-freed.

    Free frees it immediately. Should be done on released regions. *)

val pp_region : 'a region Fmt.t

val region_addr : 'a region -> int64 * int64
(** The kernel address, and the physical address, of the region. *)

val region_is_mapped : 'a region -> bool
(** Did we [mmap] it? *)

(** {2 Region lifecycle} *)

val region_memory : 'a region -> bigstring
(** Let's [mmap] it after all. (Idempotent.)

    {b WARNING} If the memory was also given to the hypervisor, (before or after
    the mmap), then any access will lead to a kernel fault which may trigger a
    panic or just segfault the current program. *)

val kernel_region_alloc : int -> 'a region
val kernel_region_release : 'a region -> unit
(** Release this kernel region. This means that it won't be automatically freed
    when the corresponding file descriptor (reg->fd) is closed.

    {b WARNING} This need to be done before donating or sharing any memory with
    the hypervisor (or a KVM guest), otherwise the kernel may become unstable. *)

val kernel_region_free : 'a region -> unit
(** Free this kernel region. This means that won't be automatically freed
    when the corresponding file descriptor (reg->fd) is closed.

    {b WARNING} Only free memory fully owned by the kernel. Freeing memory shared
    or donated away will lead to kernel instability. *)

val kernel_region_share_hyp : 'a region -> unit
val kernel_region_unshare_hyp : 'a region -> unit
val kernel_region_reclaim : 'a region -> unit

(** {2 Field access}

    Structured access to regions. Note that it's entirely possible to prod the
    {{!region_memory}raw memory}, too. *)

type ('a, 'b) field
(** And [('a, 'b) field] is an accessor for something interesting in an
    ['a]-region, that we interpret as ['b]. *)

val ( .@[] ) : 'a region -> ('a, 'b) field -> 'b
(** [the_region.@[a_field]] is like [get the_region a_field], but spelled
    differently. It gets the thing. *)

val ( .@[]<- ) : 'a region -> ('a, 'b) field -> 'b -> unit
(** [the_region.@[a_field] <- val] is like [set the_region a_field val], but
    spelled differently. It sets it the thing. *)

(** {2 Region-mapped pKVM structures} *)

type memcache

type registers = { regs : int64 array; sp : int64; pc : int64; pstate : int64; }
(** Some registers. *)

type fault_info = private {
  esr_el2   : int64;  (* Hyp Syndrom Register *)
  far_el2   : int64;  (* Hyp Fault Address Register *)
  hpfar_el2 : int64;  (* Hyp IPA Fault Address Register *)
  disr_el1  : int64;  (* Deferred [SError] Status Register *)
}
(** Some other registers *)

val pp_regs : registers Fmt.t
val pp_fault_info : fault_info Fmt.t

type struct_kvm
(** [struct kvm] from [kvm_host.h], with accessors to a few fields. *)

val nr_mem_slot_pages     : (struct_kvm, int64) field
val vcpu_array            : (struct_kvm, unit) field
val max_vcpus             : (struct_kvm, int32) field
val created_vcpus         : (struct_kvm, int32) field
val arch_pkvm_enabled     : (struct_kvm, bool) field
val arch_pkvm_teardown_mc : (struct_kvm, memcache) field

type struct_kvm_vcpu
(** [struct kvm_vcpu] from [kvm_host.h], with accessors to a few fields. *)

val vcpu_id       : (struct_kvm_vcpu, int32) field
val vcpu_idx      : (struct_kvm_vcpu, int32) field
val vcpu_cflags   : (struct_kvm_vcpu, int) field
val vcpu_iflags   : (struct_kvm_vcpu, int) field
val vcpu_features : (struct_kvm_vcpu, unit) field
val vcpu_hcr_el2  : (struct_kvm_vcpu, int64) field
val vcpu_fault    : (struct_kvm_vcpu, fault_info) field
val vcpu_regs     : (struct_kvm_vcpu, registers) field
val vcpu_fp_regs  : (struct_kvm_vcpu, unit) field
val vcpu_memcache : (struct_kvm_vcpu, memcache) field

(** {1 Higher-level operations}

    All implemented in terms of the above. *)

val map_region_guest : struct_kvm_vcpu region -> 'a region -> int64 -> unit
(** Maps a region into the guest.

    {b Warning} Must be invoked in a [vcpu-load]...[vcpu-put] block. *)

type vm = { handle : int; mem : struct_kvm region }
(** A guest. *)

type vcpu = { idx : int; mem : struct_kvm_vcpu region; vm : vm }
(** A VCPU *)

val init_vm : ?protected:bool -> unit -> vm
val teardown_vm : vm -> unit

val init_vcpu : vm -> int ->  vcpu
val teardown_vcpu : vcpu -> unit

val vcpu_load : vcpu -> unit
val vcpu_put : unit -> unit

val vcpu_set_dirty : vcpu -> unit
val vcpu_run : vcpu -> int
val vcpu_sync_state : unit -> unit

val topup_hyp_memcache : memcache -> int -> unit
val free_hyp_memcache : memcache -> unit
val topup_vcpu_memcache : vcpu -> int -> unit

val set_vcpu_regs : vcpu -> registers -> unit
val get_vcpu_regs : vcpu -> registers

type cond

module Cond : sig
  val exit : (int -> bool) -> cond
  val exit_is : int -> cond
  val fault : (fault_info -> bool) -> cond
  val regs : (registers -> bool) -> cond
  val (&&&) : cond -> cond -> cond
  val (|||) : cond -> cond -> cond
end

val vcpu_run_expect : ?cond:cond -> vcpu -> unit
(** Steps {{!vcpu_run}} until it returns something other than 0.

    Check the optional expected condition [cond]. Raises on failure. *)
