open Bigarray
include Pkvm_types
open Pkvm_c_constants
open Pkvm_proxy_utils

module Log = (val Logs.(Src.create "Pkvm_proxy" |> src_log))

let pkvm =
  let log_cfg = setup_early_log () in
  let open Unix in
  match openfile "/sys/kernel/debug/pkvm_proxy" [O_RDWR] 0 with 
  | exception Unix_error(ENOENT, _, _) ->
      Log.err (fun k -> k "Cannot find pkvm proxy — is the kernel patched?");
      exit 1
  | exception Unix_error(EACCES, _, _) ->
      Log.err (fun k -> k "Cannot open pkvm proxy — we need to run as root!");
      exit 1
  | fd -> reset_early_log log_cfg; fd

(** FFI **)

exception Proxy of Unix.error
exception HVC of Unix.error

let _ = Callback.register_exception "Pkvm.Proxy" (Proxy Unix.E2BIG)
let _ = Printexc.register_printer @@ function
  | Proxy err -> Some (Fmt.str "pKVM-proxy error: %s" (Unix.error_message err))
  | HVC err -> Some (Fmt.str "pKVM HVC error: %s" (Unix.error_message err))
  | _ -> None

type ('a, 'b) c_array = ('a, 'b, Bigarray.c_layout) Array1.t

external _ioctl  : Unix.file_descr -> int -> ('a, 'b) c_array -> int = "caml_pkvm_ioctl"
external _ioctl_int : Unix.file_descr -> int -> int -> int = "caml_pkvm_ioctl_immediate"

external super_unsafe_ba_mmap : Unix.file_descr -> int -> (char, int8_unsigned_elt) c_array = "caml_super_unsafe_ba_mmap"
external ba_munmap : (char, int8_unsigned_elt) c_array -> unit = "caml_ba_munmap" [@@warning "-32"]
external bzero : ('a, 'b) c_array -> unit = "caml_ba_bzero"

(** ioctls **)

let ioc dir ty nr size =
  ((match dir with `None -> 0 | `Wr -> 1 | `Rd -> 2 | `RdWr -> 3) lsl 30) lor
  ((size land 0x3fff) lsl 16) lor (Char.code ty lsl 8) lor (nr land 0xff)

let ioctl fd dir ty nr ba = _ioctl fd (ioc dir ty nr (Array1.size_in_bytes ba)) ba
let ioctl_int fd ty nr = _ioctl_int fd (ioc `None ty nr 0)

let returns_0 x = if x <> 0 then Fmt.failwith "ioctl: expected 0 but got %d" x

let ioctl_io fd ty nr = ioctl_int fd ty nr 0
let ioctl_ior fd ty nr kind =
  let ba = Array1.create kind c_layout 1 in
  ioctl fd `Rd ty nr ba |> returns_0;
  ba.{0}

let _empty_int64 = Array1.create int64 c_layout 0

let hvc_raw hvc_nr = function
| [||] -> ioctl pkvm `Wr 'h' hvc_nr _empty_int64
| xs   -> ioctl pkvm `Wr 'h' hvc_nr (Array1.of_array int64 c_layout xs)

let smccc_func_number func = match Pkvm_c_constants.smccc_func_number func with
| None -> failwith "hvc: host smccc function not implemented (unknown number)"
| Some nr -> nr

let hvc (type a): a host_smccc_func -> a = fun func ->
  let open Int64 in
  let nr = smccc_func_number func in
  try match func with
  | Pkvm_host_share_hyp x                   -> hvc_raw nr [|x|] |> returns_0
  | Pkvm_host_unshare_hyp x                 -> hvc_raw nr [|x|] |> returns_0
  | Pkvm_host_reclaim_page x                -> hvc_raw nr [|x|] |> returns_0
  | Pkvm_host_map_guest (phys, gphys)       -> hvc_raw nr [|phys; gphys|] |> returns_0
  | Kvm_adjust_pc vcpu_kaddr                -> hvc_raw nr [|vcpu_kaddr|] |> ignore
  | Kvm_vcpu_run vcpu_kaddr                 -> hvc_raw nr [|vcpu_kaddr|]
  | Kvm_timer_set_cntvoff cntvoff           -> hvc_raw nr [|cntvoff|] |> ignore
  | Pkvm_init_vm (host, hyp, pgd, last_ran) -> hvc_raw nr [|host; hyp; pgd; last_ran|]
  | Pkvm_init_vcpu (hdl, host, hyp)         -> hvc_raw nr [|of_int hdl; host; hyp|] |> returns_0
  | Pkvm_teardown_vm hdl                    -> hvc_raw nr [|of_int hdl|] |> returns_0
  | Pkvm_vcpu_load (hdl, idx, hcr_el2)      -> hvc_raw nr [|of_int hdl; of_int idx; hcr_el2|] |> ignore
  | Pkvm_vcpu_put                           -> hvc_raw nr [||] |> returns_0
  | Pkvm_vcpu_sync_state                    -> hvc_raw nr [||] |> returns_0
  | _ -> failwith "hvc: host smccc function not implemented"
  with Proxy err -> raise (HVC err)

let fd_of_int : int -> Unix.file_descr = Obj.magic
let int_of_fd : Unix.file_descr -> int = Obj.magic

type alloc_type = VMALLOC | PAGES_EXACT
let int_of_alloc_type = function VMALLOC -> 0 | PAGES_EXACT -> 1
let alloc_region ty sz = ioctl_int pkvm 'a' (int_of_alloc_type ty) sz |> fd_of_int

let alloc_kaddr   fd = ioctl_ior fd 'A' 0 int64
let alloc_phys    fd = ioctl_ior fd 'A' 1 int64
let alloc_release fd = ioctl_io  fd 'A' 2 |> returns_0
let alloc_free    fd = ioctl_io  fd 'A' 3 |> returns_0

let struct_kvm_size            = ioctl_io  pkvm 's' 0
let struct_kvm_get_offset      = ioctl_int pkvm 's' 1
let hyp_vm_size                = ioctl_io  pkvm 's' 2
let pgd_size                   = ioctl_io  pkvm 's' 3
let struct_kvm_vcpu_size       = ioctl_io  pkvm 's' 4
let struct_kvm_vcpu_get_offset = ioctl_int pkvm 's' 5
let hyp_vcpu_size              = ioctl_io  pkvm 's' 6

let topup_memcache_ptr ptr min_pages =
  if min_pages < 0 || 0xff <= min_pages then invalid_arg __FUNCTION__ else
  ioctl pkvm `Wr 'm' min_pages ptr |> returns_0

(* XXX extended hyp-proxy *)
let decode_memcache_ptr ptr = ioctl pkvm `Wr 'm' 0xff ptr |> returns_0

(** Regions **)

type 'a region = {
  size  : int;
  fd    : Unix.file_descr;
  kaddr : int64;
  phys  : int64;
  mmap  : bigstring Lazy.t;
  __xx  : 'a -> unit;  (* GAAAH hammer the phantom 'a out of covariance *)
}

(** Structure R/W **)

(* Typed accessors to components of kernel-managed structures. *)
type 'a rd = bigstring -> int -> 'a
type 'a wr = bigstring -> int -> 'a -> unit
type ('a, 'b) field = F of int * 'b rd * 'b wr

let ( .@[] ) (type a) (rg : a region) (F (off, r, _) : (a, _) field) = r (Lazy.force rg.mmap) off
let ( .@[]<- ) (type a) (rg : a region) (F (off, _, w) : (a, _) field) v = w (Lazy.force rg.mmap) off v

type memcache = int64 * int

type registers = { regs : int64 array; sp : int64; pc : int64; pstate : int64; }

type fault_info = {
  esr_el2   : int64; (* Hyp Syndrom Register *)
  far_el2   : int64; (* Hyp Fault Address Register *)
  hpfar_el2 : int64; (* Hyp IPA Fault Address Register *)
  disr_el1  : int64; (* Deferred [SError] Status Register *)
}

let (@+) a i = a + i * sizeof___u64

let read_array_int64 s i n = Array.init n (fun j -> Bigstring.get_int64 s (i @+ j))
and write_array_int64 s i = Array.iteri (fun j -> Bigstring.set_int64 s (i @+ j))

let read_regs s i =
  let open Bigstring in
  { regs   = read_array_int64 s i 31;
    sp     = get_int64 s (i @+ 31);
    pc     = get_int64 s (i @+ 32);
    pstate = get_int64 s (i @+ 33); }

let write_regs s i r =
  let open Bigstring in
  write_array_int64 s i r.regs;
  set_int64 s (i @+ 31) r.sp;
  set_int64 s (i @+ 32) r.pc;
  set_int64 s (i @+ 33) r.pstate

let read_fault_info s i =
  let open Bigstring in
  { esr_el2   = get_int64 s (i @+ 0);
    far_el2   = get_int64 s (i @+ 1);
    hpfar_el2 = get_int64 s (i @+ 2);
    disr_el1  = get_int64 s (i @+ 3); }

let read_memcache s i =
  let open Bigstring in
  (get_int64 s (i + memcache_head_off),
   get_int64 s (i + memcache_nr_off) |> Int64.to_int)

let write_memcache s i (head, n) =
  let open Bigstring in
  set_int64 s (i + memcache_head_off) head;
  set_int64 s (i + memcache_nr_off) (Int64.of_int n)

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
let arch_pkvm_teardown_mc = struct_kvm 5 (read_memcache, write_memcache)

type struct_kvm_vcpu

let struct_kvm_vcpu enum (r, w) : (struct_kvm_vcpu, _) field =
  F (struct_kvm_vcpu_get_offset enum, r, w)
let vcpu_id       = struct_kvm_vcpu 0 f_int32
let vcpu_idx      = struct_kvm_vcpu 1 f_int32
let vcpu_cflags   = struct_kvm_vcpu 2 f_int8
let vcpu_iflags   = struct_kvm_vcpu 3 f_int8
let vcpu_features = struct_kvm_vcpu 4 f_int8
let vcpu_hcr_el2  = struct_kvm_vcpu 5 f_int64
let vcpu_fault    = struct_kvm_vcpu 6 (read_fault_info, not_implemented)
let vcpu_regs     = struct_kvm_vcpu 7 (read_regs, write_regs)
let vcpu_fp_regs  = struct_kvm_vcpu 8 f_not_implemented
let vcpu_memcache = struct_kvm_vcpu 9 (read_memcache, write_memcache)

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
| Kvm_adjust_pc vcpu_kaddr           -> Fmt.pf ppf "KVM_ADJUST_PC (0x%Lx)" vcpu_kaddr
| Kvm_vcpu_run vcpu_kaddr            -> Fmt.pf ppf "KVM_VCPU_RUN (0x%Lx)" vcpu_kaddr
| Kvm_timer_set_cntvoff cntvoff      -> Fmt.pf ppf "KVM_TIMER_SET_CNTVOFF (0x%Lx)" cntvoff
| Vgic_v3_save_vmcr_aprs             -> Fmt.pf ppf "VGIC_V3_SAVE_VMCR_APRS"
| Vgic_v3_restore_vmcr_aprs          -> Fmt.pf ppf "VGIC_V3_RESTORE_VMCR_APRS"
| Pkvm_init_vm (host, hyp, pgd, ran) -> Fmt.pf ppf "PKVM_INIT_VM (@[0x%Lx,@ 0x%Lx,@ 0x%Lx,@ 0x%Lx@])" host hyp pgd ran
| Pkvm_init_vcpu (hdl, host, hyp)    -> Fmt.pf ppf "PKVM_INIT_VCPU (@[%d,@ 0x%Lx,@ 0x%Lx@])" hdl host hyp
| Pkvm_teardown_vm hdl               -> Fmt.pf ppf "PKVM_TEARDOWN_VM %d" hdl
| Pkvm_vcpu_load (hdl, idx, hcr_el2) -> Fmt.pf ppf "PKVM_VCPU_LOAD (@[%d,@ %d,@ 0x%Lx@])" hdl idx hcr_el2
| Pkvm_vcpu_put                      -> Fmt.pf ppf "PKVM_VCPU_PUT"
| Pkvm_vcpu_sync_state               -> Fmt.pf ppf "PKVM_VCPU_SYNC_STATE"

let pp_regs ppf rg =
  let px ppf = Fmt.pf ppf "0x%Lx" in
  Fmt.pf ppf "@[<v1>[regs: @[%a@],@ sp: 0x%Lx,@ pc: 0x%Lx,@ pstate: 0x%Lx]@]"
  Fmt.(array ~sep:sp px) rg.regs rg.sp rg.pc rg.pstate

let pp_fault_info ppf info =
  Fmt.pf ppf "@[<1>[esr: 0x%Lx,@ far: 0x%Lx,@ hpfar: 0x%Lx,@ disr: 0x%Lx]@]"
  info.esr_el2 info.far_el2 info.hpfar_el2 info.disr_el1

let pp_memcache ppf (head, nr) = Fmt.pf ppf "@[<3>mc(0x%Lx, %d)@]" head nr

(** Higher-level ops **)

let (//) a b = (a + b - 1) / b

let hvc func = Log.info (fun k -> k "hvc %a" pp_host_smccc_func func); hvc func

type vm = { handle : int; vcpus: int; mem : struct_kvm region }
type vcpu = { idx : int; mem : struct_kvm_vcpu region; vm : vm }

let topup_memcache mc min =
  let buf = Bigstring.create sizeof_hprox_memcache in
  write_memcache buf 0 mc;
  topup_memcache_ptr buf min;
  let mc1 = read_memcache buf 0 in
  Log.debug (fun k -> k "memcache %a %d -> %a" pp_memcache mc min pp_memcache mc1);
  mc1

let free_memcache mc = topup_memcache mc 0 |> ignore

let decode_memcache (_, n as mc) =
  let buf = Bigstring.create (sizeof_hprox_memcache + n * sizeof___u64) in
  write_memcache buf 0 mc;
  decode_memcache_ptr buf;
  read_array_int64 buf sizeof_hprox_memcache n

let topup_vcpu_memcache { mem; _ } min =
  mem.@[vcpu_memcache] <- topup_memcache mem.@[vcpu_memcache] min

module Region = struct

  let kaddr reg = reg.kaddr
  and phys reg = reg.phys
  let size reg = reg.size
  let is_mapped reg = Lazy.is_val reg.mmap
  let memory { mmap = lazy mmap; _ } = mmap
  let bzero reg = bzero (memory reg)

  let pp ppf reg =
    Fmt.pf ppf "[region @[fd: %d,@ size: %d,@ kaddr: 0x%Lx,@ phys: 0x%Lx%a@]]"
    (int_of_fd reg.fd) reg.size reg.kaddr reg.phys
    (fun ppf x -> if Lazy.is_val x then Fmt.pf ppf ",@ mapped" else ()) reg.mmap

  let release reg =
    Log.debug (fun k -> k "Region.release@ %a" pp reg);
    alloc_release reg.fd

  let close reg = Unix.close reg.fd

  (* Note — bigarrays are unmapped on finalisation. *)
  let free reg =
    Log.debug (fun k -> k "Region.free@ %a" pp reg);
    alloc_free reg.fd;
    Unix.close reg.fd

  let alloc ?init ?release:(do_release = true) size =
    let fd    = alloc_region PAGES_EXACT size in
    let kaddr = alloc_kaddr fd
    and phys  = alloc_phys fd
    and mmap  = lazy (super_unsafe_ba_mmap fd size) in
    let res = { fd; size; kaddr; phys; mmap; __xx = ignore } in
    Log.debug (fun k -> k "Region.alloc %d ->@ %a" size pp res);
    Option.fold init ~none:() ~some:(fun s ->
      Bigstring.blit_from_string ~n:(String.length s |> min size) 
        s (Lazy.force mmap));
    if do_release then release res;
    res
end

let page_size  = 0x1000
and page_shift = 12

let for_each_page ?(base = 0L) size f =
  for i = 0 to size // page_size - 1 do
    f Int64.((base lsr page_shift) + of_int i)
  done

let host_share_hyp reg =
  Log.debug (fun k -> k "host_share_hyp@ %a" Region.pp reg);
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_share_hyp pg)

let host_unshare_hyp reg =
  Log.debug (fun k -> k "host_unshare_hyp@ %a" Region.pp reg);
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_unshare_hyp pg)

let host_reclaim_region reg =
  Log.debug (fun k -> k "host_reclaim_region@ %a" Region.pp reg);
  for_each_page ~base:reg.phys reg.size @@ fun pg -> hvc (Pkvm_host_reclaim_page pg)

let host_map_guest ?(topup_memcache = true) vcpu reg guest_phys =
  let open Int64 in
  let phys  = reg.phys lsr page_shift
  and gphys = guest_phys lsr page_shift
  and mc = 5 * (reg.size // page_size) in
  Log.debug (fun k -> k "host_map_guest@ %a" Region.pp reg);
  if topup_memcache then topup_vcpu_memcache vcpu mc;
  for_each_page reg.size @@ fun pg -> hvc (Pkvm_host_map_guest (phys + pg, gphys + pg))

let nr_cpus = 128

(* Memory:
   - All regions are released, to avoid freeing them while in use if the FD is
     lost (we crash, for instance).
   - host_kvm is shared, unshared, and then explicitly freed.
   - Other 3 structures are donated to the hypervisor. They later reappear as
     pages in teardown memcache, and freed that way.
*)
let init_vm ?(vcpus = 1) ?(protected = true) () =
  let release = true in
  let host_kvm = Region.alloc ~release struct_kvm_size
  and hyp_vm   = Region.alloc ~release (hyp_vm_size + vcpus * sizeof_void_p)
  and pgd      = Region.alloc ~release pgd_size
  and last_ran = Region.alloc ~release (nr_cpus * sizeof_int) in

  Region.bzero host_kvm;
  host_kvm.@[arch_pkvm_enabled] <- protected;
  host_kvm.@[created_vcpus] <- Int32.of_int vcpus;
  host_share_hyp host_kvm;

  match hvc (Pkvm_init_vm (host_kvm.kaddr, hyp_vm.kaddr, pgd.kaddr, last_ran.kaddr)) with
  | handle ->
      List.iter Region.close [hyp_vm; pgd; last_ran];
      Log.debug (fun k -> k "init_vm ->@ %d@ %a" handle Region.pp host_kvm);
      { handle; vcpus; mem = host_kvm }
  | exception exn ->
      host_unshare_hyp host_kvm;
      List.iter Region.free [hyp_vm; pgd; last_ran];
      Region.free host_kvm;
      raise exn

let teardown_vm ?free_memcache:(free_mc = true) vm =
  Log.debug (fun k -> k "teardown_vm %d@ %a" vm.handle Region.pp vm.mem);
  hvc (Pkvm_teardown_vm vm.handle);
  host_unshare_hyp vm.mem;
  if free_mc then free_memcache vm.mem.@[arch_pkvm_teardown_mc];
  Region.free vm.mem

let init_vcpu vm idx =
  let release = true in
  let host_vcpu = Region.alloc ~release struct_kvm_vcpu_size
  and hyp_vcpu  = Region.alloc ~release (hyp_vcpu_size + vm.vcpus * sizeof_void_p) in

  Region.bzero host_vcpu;
  host_vcpu.@[vcpu_idx] <- Int32.of_int idx;
  host_vcpu.@[vcpu_hcr_el2] <- Int64.(1L lsl 31);
  host_share_hyp host_vcpu;

  match hvc (Pkvm_init_vcpu (vm.handle, host_vcpu.kaddr, hyp_vcpu.kaddr)) with
  | () ->
      Region.close hyp_vcpu;
      Log.debug (fun k -> k "init_vcpu %d %d ->@ %a" vm.handle idx Region.pp host_vcpu);
      { idx; mem = host_vcpu; vm }
  | exception exn ->
      host_unshare_hyp host_vcpu;
      Region.free hyp_vcpu;
      Region.free host_vcpu;
      raise exn

let free_vcpu ?free_memcache:(free_mc = true) vcpu =
  host_unshare_hyp vcpu.mem;
  if free_mc then free_memcache vcpu.mem.@[vcpu_memcache];
  Region.free vcpu.mem

let vcpu_load vcpu = hvc (Pkvm_vcpu_load (vcpu.vm.handle, vcpu.idx, 0L))
let vcpu_put () = hvc Pkvm_vcpu_put

let vcpu_set_dirty vcpu = vcpu.mem.@[vcpu_iflags] <- 0x80
let vcpu_adjust_pc vcpu = hvc (Kvm_adjust_pc vcpu.mem.kaddr)
let vcpu_run vcpu = hvc (Kvm_vcpu_run vcpu.mem.kaddr)
let vcpu_sync_state () = hvc Pkvm_vcpu_sync_state

let timer_set_cntvoff cntvoff = hvc (Kvm_timer_set_cntvoff cntvoff)

let set_vcpu_regs vcpu regs =
  vcpu.mem.@[vcpu_regs] <- regs; vcpu_set_dirty vcpu
let get_vcpu_regs vcpu = vcpu.mem.@[vcpu_regs]
