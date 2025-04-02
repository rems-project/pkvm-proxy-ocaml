(* Host hypercalls. `enum __kvm_host_smccc_func`, and their ioctl arguments.
 *
 * Keep in sync with `hypercalls.h`.
 *)
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
  | Kvm_adjust_pc          : int64 -> unit host_smccc_func
  | Kvm_vcpu_run           : int64 -> int host_smccc_func
  | Kvm_timer_set_cntvoff  : int64 -> unit host_smccc_func
  | Vgic_v3_save_vmcr_aprs
  | Vgic_v3_restore_vmcr_aprs
  | Pkvm_init_vm           : int64 * int64 * int64 * int64 -> int host_smccc_func
  | Pkvm_init_vcpu         : int * int64 * int64 -> unit host_smccc_func
  | Pkvm_teardown_vm       : int -> unit host_smccc_func
  | Pkvm_vcpu_load         : int * int * int64 -> unit host_smccc_func
  | Pkvm_vcpu_put          : unit host_smccc_func
  | Pkvm_vcpu_sync_state   : unit host_smccc_func

