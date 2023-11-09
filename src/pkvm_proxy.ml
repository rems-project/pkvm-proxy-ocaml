open Bigarray

let pkvm = Unix.(openfile "/sys/kernel/debug/pkvm_proxy" [O_RDWR] 0)

(** Hyp-proxy types **)

(* From arch/arm64/include/asm/kvm_asm.h *)
type arm_exception = EXCEPTION_IRQ | EXCEPTION_EL1_SERROR | EXCEPTION_TRAP | EXCEPTION_IL
let arm_exception_of_int : int -> arm_exception = Obj.magic

(** Host hypercalls. `enum __kvm_host_smccc_func`, and their ioctl arguments. **)
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
  | Kvm_vcpu_run           : int64 -> arm_exception host_smccc_func
  | Kvm_timer_set_cntvoff
  | Vgic_v3_save_vmcr_aprs
  | Vgic_v3_restore_vmcr_aprs
  | Pkvm_init_vm           : int64 * int64 * int64 * int64 -> int host_smccc_func
  | Pkvm_init_vcpu         : int * int64 * int64 -> unit host_smccc_func
  | Pkvm_teardown_vm       : int -> unit host_smccc_func
  | Pkvm_vcpu_load         : int * int * int64 -> unit host_smccc_func
  | Pkvm_vcpu_put          : unit host_smccc_func
  | Pkvm_vcpu_sync_state   : unit host_smccc_func

type registers = { regs : int64 array; sp : int64; pc : int64; pstate : int64; }

type fault_info = {
  esr_el2   : int64; (* Hyp Syndrom Register *)
  far_el2   : int64; (* Hyp Fault Address Register *)
  hpfar_el2 : int64; (* Hyp IPA Fault Address Register *)
  disr_el1  : int64; (* Deferred [SError] Status Register *)
}

(** The missing bits **)

type ('a, 'b) c_array = ('a, 'b, Bigarray.c_layout) Array1.t

module Bigstring = struct
  type t = (char, int8_unsigned_elt) c_array
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
    let n = match n with Some n -> n | _ -> String.length src - src_i in
    if Array1.dim dst - dst_i < n || String.length src - src_i < n then
      invalid_arg "Bigstring.blit_string: too many bytes to blit.";
    for i = 0 to n - 1 do
      dst.{dst_i + i} <- src.[src_i + i]
    done
  let hex ?w () ppf s = Fmt.hex ?w () ppf (Array1.dim s, Array1.get s)
end

let (//) a b = (a + b - 1) / b

module Int64 = struct
  include Int64
  let (+) = add
  let (lsr) = shift_right_logical
  let (lsl) = shift_left
end

(** FFI **)

external sizes : unit -> int array = "caml_sizes"
let sizeof___u64, sizeof_int, sizeof_void_p, sizeof_memcache =
  let ss = sizes() in
  ss.(0), ss.(1), ss.(2), ss.(3)

type ioc_dir = IOC_NONE | IOC_WRITE | IOC_READ | IOC_READ_WRITE (* Order is important! *)
external _IOC : ioc_dir -> char -> int -> int -> int = "caml__IOC" [@@noalloc]

external ioctl_0  : Unix.file_descr -> int -> int = "caml_pkvm_ioctl"
external ioctl_64 : Unix.file_descr -> int -> int -> int = "caml_pkvm_ioctl_long"
external ioctl_p  : Unix.file_descr -> int -> ('a, 'b) c_array -> int = "caml_pkvm_ioctl_ptr"

external super_unsafe_ba_mmap : Unix.file_descr -> int -> (char, int8_unsigned_elt) c_array = "caml_super_unsafe_ba_mmap"
external ba_munmap : (char, int8_unsigned_elt) c_array -> unit = "caml_ba_munmap"
external bzero : ('a, 'b) c_array -> unit = "caml_ba_bzero"

external _sched_setaffinity : int -> int array -> unit = "caml_sched_setaffinity"
let sched_setaffinity ?(thread = 0) = _sched_setaffinity thread

(** ioctls **)

(* Unlike in C, the size argument `sz` is not a string representing the type,
   but the actual sizeof() that type. *)
let _IO   ty nr    = _IOC IOC_NONE ty nr 0
let _IOR  ty nr sz = _IOC IOC_READ ty nr sz
let _IOW  ty nr sz = _IOC IOC_WRITE ty nr sz
let _IOWR ty nr sz = _IOC IOC_READ_WRITE ty nr sz

let returns_0 x = if x <> 0 then Fmt.failwith "ioctl: expected 0 but got %d" x

let ioctl_p_wr fd n k v =
  let p = Array1.create k c_layout 1 in
  p.{0} <- v;
  ioctl_p fd n p

let ioctl_p_wr_vec fd n k vs =
  ioctl_p fd n (Array1.of_array k c_layout vs)

let ioctl_p_rd fd n k =
  let p = Array1.create k c_layout 1 in
  ioctl_p fd n p |> returns_0;
  p.{0}

let proxy_ioctl hvcnum numarg = _IOW 'h' hvcnum (8 * numarg)

let hvc (type a): a host_smccc_func -> a = function
| Pkvm_host_share_hyp x ->
    ioctl_p_wr pkvm (proxy_ioctl 13 1) int64 x |> returns_0
| Pkvm_host_unshare_hyp x ->
    ioctl_p_wr pkvm (proxy_ioctl 14 1) int64 x |> returns_0
| Pkvm_host_reclaim_page x ->
    ioctl_p_wr pkvm (proxy_ioctl 15 1) int64 x |> returns_0
| Pkvm_host_map_guest (phys, gphys) ->
    ioctl_p_wr_vec pkvm (proxy_ioctl 16 2) int64 [|phys; gphys|] |> returns_0
| Kvm_vcpu_run vcpu_kaddr ->
    ioctl_p_wr pkvm (proxy_ioctl 18 1) int64 vcpu_kaddr |> arm_exception_of_int
| Pkvm_init_vm (host, hyp, pgd, last_ran) ->
    ioctl_p_wr_vec pkvm (proxy_ioctl 22 4) int64 [|host; hyp; pgd; last_ran|]
| Pkvm_init_vcpu (hdl, host, hyp) ->
    let hdl = Int64.of_int hdl in
    ioctl_p_wr_vec pkvm (proxy_ioctl 23 3) int64 [|hdl; host; hyp|] |> returns_0
| Pkvm_teardown_vm hdl ->
    ioctl_p_wr pkvm (proxy_ioctl 24 1) int64 (Int64.of_int hdl) |> ignore
| Pkvm_vcpu_load (hdl, idx, hcr_el2) ->
    let hdl, idx = Int64.(of_int hdl, of_int idx) in
    ioctl_p_wr_vec pkvm (proxy_ioctl 25 3) int64 [| hdl; idx; hcr_el2 |] |> ignore
| Pkvm_vcpu_put ->
    ioctl_0 pkvm (proxy_ioctl 26 0) |> returns_0
| Pkvm_vcpu_sync_state ->
    ioctl_0 pkvm (proxy_ioctl 27 0) |> returns_0
| _ -> failwith "hvc: host smccc function not implemented"

type alloc_type = VMALLOC | PAGES_EXACT (* Order! *)
let alloc (a: alloc_type) = _IO 'a' (Obj.magic a)

let fd_of_int : int -> Unix.file_descr = Obj.magic
let int_of_fd : Unix.file_descr -> int = Obj.magic
let alloc_pages sz = ioctl_64 pkvm (alloc PAGES_EXACT) sz |> fd_of_int

let alloc_kaddr   fd = ioctl_p_rd fd (_IOR 'A' 0 sizeof___u64) int64
let alloc_phys    fd = ioctl_p_rd fd (_IOR 'A' 1 sizeof___u64) int64
let alloc_release fd = ioctl_0    fd (_IO  'A' 2) |> returns_0
let alloc_free    fd = ioctl_0    fd (_IO  'A' 3) |> returns_0

let struct_kvm_size            = ioctl_0  pkvm (_IO 's' 0)
let struct_kvm_get_offset      = ioctl_64 pkvm (_IO 's' 1)
let hyp_vm_size                = ioctl_0  pkvm (_IO 's' 2)
let pgd_size                   = ioctl_0  pkvm (_IO 's' 3)
let struct_kvm_vcpu_size       = ioctl_0  pkvm (_IO 's' 4)
let struct_kvm_vcpu_get_offset = ioctl_64 pkvm (_IO 's' 5)
let hyp_vcpu_size              = ioctl_0  pkvm (_IO 's' 6)

let memcache_topup n = _IOWR 'm' n sizeof_memcache
let topup_hyp_memcache ptr min_pages = ioctl_p pkvm (memcache_topup min_pages) ptr |> returns_0
let free_hyp_memcache ptr = topup_hyp_memcache ptr 0

(** Regions **)

(** A kernel memory region.
    The type parameter is phantom and reflects the structured data in the region
    (if any). The parameter is inferred from record-field accesses. **)
type 'a region = {
  size  : int;
  fd    : Unix.file_descr;
  kaddr : int64;
  phys  : int64;
  mmap  : Bigstring.t Lazy.t;
  __xx  : 'a -> unit;  (* GAAAH hammer the phantom 'a out of covariance *)
}

let region_is_mapped reg = Lazy.is_val reg.mmap
let region_memory { mmap = lazy mmap; _ } = mmap

(** Structure R/W **)

let (@+) a i = a + i * sizeof___u64

let read_regs s i =
  let open Bigstring in
  { regs   = Array.init 31 (fun j -> get_int64 s (i @+ j));
    sp     = get_int64 s (i @+ 31);
    pc     = get_int64 s (i @+ 32);
    pstate = get_int64 s (i @+ 33); }

let write_regs s i r =
  let open Bigstring in
  Array.iteri (fun j -> set_int64 s (i @+ j)) r.regs;
  set_int64 s (i @+ 31) r.sp;
  set_int64 s (i @+ 32) r.pc;
  set_int64 s (i @+ 33) r.pstate

let read_fault_info s i =
  let open Bigstring in
  { esr_el2   = get_int64 s (i @+ 0);
    far_el2   = get_int64 s (i @+ 1);
    hpfar_el2 = get_int64 s (i @+ 2);
    disr_el1  = get_int64 s (i @+ 3); }

(* memcache is represented by a (bigarray-wrapped) pointer; offset computation
   corresponds to extracting the array slice. *)
let read_memcache s i = Array1.sub s i sizeof_memcache

(* Typed accessors to components of kernel-managed structures. *)
type 'a rd = Bigstring.t -> int -> 'a
type 'a wr = Bigstring.t -> int -> 'a -> unit
type ('a, 'b) field = F of int * 'b rd * 'b wr

let not_implemented _ = failwith "structure field conversion not implemented"
let f_int64 = Bigstring.(get_int64, set_int64)
let f_int32 = Bigstring.(get_int32, set_int32)
let f_int8  = Bigstring.(get_uint8, set_uint8)
let f_bool  = Bigstring.(get_bool, set_bool)
let f_not_implemented: unit rd * unit wr = not_implemented, not_implemented

type struct_kvm
let struct_kvm enum (r, w) : (struct_kvm, _) field =
  F (struct_kvm_get_offset enum, r, w)
let nr_mem_slot_pages     = struct_kvm 0 f_int64
let vcpu_array            = struct_kvm 1 f_not_implemented
let max_vcpus             = struct_kvm 2 f_int32
let created_vcpus         = struct_kvm 3 f_int32
let arch_pkvm_enabled     = struct_kvm 4 f_bool
let arch_pkvm_teardown_mc = struct_kvm 5 (read_memcache, not_implemented)

type struct_kvm_vcpu
let struct_kvm_vcpu enum (r, w) : (struct_kvm_vcpu, _) field =
  F (struct_kvm_vcpu_get_offset enum, r, w)
let vcpu_id       = struct_kvm_vcpu 0 f_int32
let vcpu_idx      = struct_kvm_vcpu 1 f_int32
let vcpu_cflags   = struct_kvm_vcpu 2 f_int8
let vcpu_iflags   = struct_kvm_vcpu 3 f_int8
let vcpu_features = struct_kvm_vcpu 4 f_not_implemented
let vcpu_hcr_el2  = struct_kvm_vcpu 5 f_int64
let vcpu_fault    = struct_kvm_vcpu 6 (read_fault_info, not_implemented)
let vcpu_regs     = struct_kvm_vcpu 7 (read_regs, write_regs)
let vcpu_fp_regs  = struct_kvm_vcpu 8 f_not_implemented
let vcpu_memcache = struct_kvm_vcpu 9 (read_memcache, not_implemented)

let ( .@[] ) (type a) (rg : a region) (F (off, r, _) : (a, _) field) = r (Lazy.force rg.mmap) off
let ( .@[]<- ) (type a) (rg : a region) (F (off, _, w) : (a, _) field) v = w (Lazy.force rg.mmap) off v

(* Printers *)

let pp_host_smccc_func (type a) ppf: a host_smccc_func -> _ = function
| Kvm_hyp_init                       -> Fmt.pf ppf "KVM_HYP_INIT"
| Kvm_get_mdcr_el2                   -> Fmt.pf ppf "KVM_GET_MDCR_EL2"
| Pkvm_init                          -> Fmt.pf ppf "PKVM_INIT"
| Pkvm_create_private_mapping        -> Fmt.pf ppf "PKVM_CREATE_PRIVATE_MAPPING"
| Pkvm_cpu_set_vector                -> Fmt.pf ppf "PKVM_CPU_SET_VECTOR"
| Kvm_enable_ssbs                    -> Fmt.pf ppf "KVM_ENABLE_SSBS"
| Vgic_v3_init_lrs                   -> Fmt.pf ppf "VGIC_V3_INIT_LRS"
| Vgic_v3_get_gic_config             -> Fmt.pf ppf "VGIC_V3_GET_GIC_CONFIG"
| Kvm_flush_vm_context               -> Fmt.pf ppf "KVM_FLUSH_VM_CONTEXT"
| Kvm_tlb_flush_vmid_ipa             -> Fmt.pf ppf "KVM_TLB_FLUSH_VMID_IPA"
| Kvm_tlb_flush_vmid                 -> Fmt.pf ppf "KVM_TLB_FLUSH_VMID"
| Kvm_flush_cpu_context              -> Fmt.pf ppf "KVM_FLUSH_CPU_CONTEXT"
| Pkvm_prot_finalize                 -> Fmt.pf ppf "PKVM_PROT_FINALIZE"
| Pkvm_host_share_hyp x              -> Fmt.pf ppf "PKVM_HOST_SHARE_HYP (0x%Lx)" x
| Pkvm_host_unshare_hyp x            -> Fmt.pf ppf "PKVM_HOST_UNSHARE_HYP (0x%Lx)" x
| Pkvm_host_reclaim_page x           -> Fmt.pf ppf "PKVM_HOST_RECLAIM_PAGE (0x%Lx)" x
| Pkvm_host_map_guest (phys, gphys)  -> Fmt.pf ppf "PKVM_HOST_MAP_GUEST (@[0x%Lx,@ 0x%Lx@])" phys gphys
| Kvm_adjust_pc                      -> Fmt.pf ppf "KVM_ADJUST_PC"
| Kvm_vcpu_run vcpu_kaddr            -> Fmt.pf ppf "KVM_VCPU_RUN (0x%Lx)" vcpu_kaddr
| Kvm_timer_set_cntvoff              -> Fmt.pf ppf "KVM_TIMER_SET_CNTVOFF"
| Vgic_v3_save_vmcr_aprs             -> Fmt.pf ppf "VGIC_V3_SAVE_VMCR_APRS"
| Vgic_v3_restore_vmcr_aprs          -> Fmt.pf ppf "VGIC_V3_RESTORE_VMCR_APRS"
| Pkvm_init_vm (host, hyp, pgd, ran) -> Fmt.pf ppf "PKVM_INIT_VM (@[0x%Lx,@ 0x%Lx,@ 0x%Lx,@ 0x%Lx@])" host hyp pgd ran
| Pkvm_init_vcpu (hdl, host, hyp)    -> Fmt.pf ppf "PKVM_INIT_VCPU (@[%d,@ 0x%Lx,@ 0x%Lx@])" hdl host hyp
| Pkvm_teardown_vm hdl               -> Fmt.pf ppf "PKVM_TEARDOWN_VM %d" hdl
| Pkvm_vcpu_load (hdl, idx, hcr_el2) -> Fmt.pf ppf "PKVM_VCPU_LOAD (@[%d,@ %d,@ 0x%Lx@])" hdl idx hcr_el2
| Pkvm_vcpu_put                      -> Fmt.pf ppf "PKVM_VCPU_PUT"
| Pkvm_vcpu_sync_state               -> Fmt.pf ppf "PKVM_VCPU_SYNC_STATE"

let pp_arm_exception ppf exn = Fmt.string ppf @@ match exn with
  | EXCEPTION_IRQ        -> "EXCEPTION_IRQ"
  | EXCEPTION_EL1_SERROR -> "EXCEPTION_EL1_SERROR"
  | EXCEPTION_TRAP       -> "EXCEPTION_TRAP"
  | EXCEPTION_IL         -> "EXCEPTION_IL"

let pp_regs ppf rg =
  let px ppf = Fmt.pf ppf "0x%Lx" in
  Fmt.pf ppf "@[<v1>[regs: @[%a@],@ sp: 0x%Lx,@ pc: 0x%Lx,@ pstate: 0x%Lx]@]"
  Fmt.(array ~sep:sp px) rg.regs rg.sp rg.pc rg.pstate

let pp_fault_info ppf info =
  Fmt.pf ppf "@[<1>[esr: 0x%Lx,@ far: 0x%Lx,@ hpfar: 0x%Lx,@ disr: 0x%Lx]@]"
  info.esr_el2 info.far_el2 info.hpfar_el2 info.disr_el1

let pp_region ppf reg =
  Fmt.pf ppf "[region @[fd: %d,@ size: %d,@ kaddr: 0x%Lx,@ phys: 0x%Lx%a@]]"
  (int_of_fd reg.fd) reg.size reg.kaddr reg.phys
  (fun ppf x -> if Lazy.is_val x then Fmt.pf ppf ",@ mapped" else ()) reg.mmap

(** Higher-level ops **)

module Log = (val Logs.(Src.create "Pkvm_proxy" |> src_log))
let hvc func = Log.debug (fun k -> k "hvc %a" pp_host_smccc_func func); hvc func

let page_size  = 4096
and page_shift = 12

let kernel_region_alloc size =
  let fd    = alloc_pages size in
  let kaddr = alloc_kaddr fd
  and phys  = alloc_phys fd
  and mmap  = lazy ( let p = super_unsafe_ba_mmap fd size in bzero p; p ) in
  let res = { fd; size; kaddr; phys; mmap; __xx = ignore } in
  Gc.finalise (fun r -> if Lazy.is_val r.mmap then ba_munmap (Lazy.force r.mmap)) res;
  Log.info (fun k -> k "kernel_region_alloc ->@ %a" pp_region res);
  res

let kernel_region_release reg = alloc_release reg.fd
let kernel_region_free reg = alloc_free reg.fd

let for_each_page ?(base = 0L) size f =
  for i = 0 to size // page_size - 1 do
    f Int64.((base lsr page_shift) + of_int i)
  done

let kernel_region_share_hyp reg =
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_share_hyp pg)

let kernel_region_unshare_hyp reg =
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_unshare_hyp pg)

let kernel_region_reclaim reg =
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_reclaim_page pg)

let num_vcpu = 1
let max_num_cpu = 16

let init_vm ?(protected = false) () =
  let host_kvm = kernel_region_alloc struct_kvm_size
  and hyp_kvm  = kernel_region_alloc (hyp_vm_size + num_vcpu * sizeof_void_p)
  and pgd      = kernel_region_alloc pgd_size
  and last_ran = kernel_region_alloc (max_num_cpu * sizeof_int) in

  List.iter kernel_region_release [host_kvm; hyp_kvm; pgd; last_ran];
  kernel_region_share_hyp host_kvm;

  host_kvm.@[arch_pkvm_enabled] <- protected;
  host_kvm.@[created_vcpus] <- Int32.of_int num_vcpu;
  let h = hvc (Pkvm_init_vm (host_kvm.kaddr, hyp_kvm.kaddr, pgd.kaddr, last_ran.kaddr)) in

  Log.info (fun k -> k "init_vm ->@ %d@ %a" h pp_region host_kvm);
  host_kvm, h

let teardown_vm handle vm =
  Log.info (fun k -> k "teardown_vm@ %d@ %a@ ->" handle pp_region vm);
  hvc (Pkvm_teardown_vm handle);
  free_hyp_memcache vm.@[arch_pkvm_teardown_mc];
  kernel_region_unshare_hyp vm;
  kernel_region_free vm

let init_vcpu handle idx =
  let host_vcpu = kernel_region_alloc struct_kvm_vcpu_size
  and hyp_vcpu  = kernel_region_alloc (hyp_vcpu_size + num_vcpu * sizeof_void_p) in

  List.iter kernel_region_release [host_vcpu; hyp_vcpu];
  kernel_region_share_hyp host_vcpu;

  host_vcpu.@[vcpu_idx] <- Int32.of_int idx;
  host_vcpu.@[vcpu_hcr_el2] <- Int64.(1L lsl 31);
  hvc (Pkvm_init_vcpu (handle, host_vcpu.kaddr, hyp_vcpu.kaddr));

  Log.info (fun k -> k "init_vcpu@ %d@ %d@ -> %a" handle idx pp_region host_vcpu);
  host_vcpu

let vcpu_set_dirty reg = reg.@[vcpu_iflags] <- 0x80
let vcpu_load handle idx = hvc (Pkvm_vcpu_load (handle, idx, 0L))
let vcpu_run vcpu = hvc (Kvm_vcpu_run vcpu.kaddr)
let vcpu_put () = hvc Pkvm_vcpu_put
let vcpu_sync_state () = hvc Pkvm_vcpu_sync_state

let map_region_guest vcpu mem gphys =
  let open Int64 in
  let phys  = mem.phys lsr page_shift
  and gphys = gphys lsr page_shift in
  for_each_page mem.size @@ fun pg ->
    topup_hyp_memcache (vcpu.@[vcpu_memcache]) 5;
    hvc (Pkvm_host_map_guest (phys + pg, gphys + pg))
