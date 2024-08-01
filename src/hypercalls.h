/* Hypercalls.
 *
 * Pasted from arch/arm64/include/asm/kvm_asm.h in the Linux sources.
 * Keep in sync with the OCaml type defn.
 */

/* PASTE -> */

#define __KVM_HOST_SMCCC_FUNC___kvm_hyp_init			0

enum __kvm_host_smccc_func {
	/* Hypercalls available only prior to pKVM finalisation */
	/* __KVM_HOST_SMCCC_FUNC___kvm_hyp_init */
	__KVM_HOST_SMCCC_FUNC___kvm_get_mdcr_el2 = __KVM_HOST_SMCCC_FUNC___kvm_hyp_init + 1,
	__KVM_HOST_SMCCC_FUNC___pkvm_init,
	__KVM_HOST_SMCCC_FUNC___pkvm_create_private_mapping,
	__KVM_HOST_SMCCC_FUNC___pkvm_cpu_set_vector,
	__KVM_HOST_SMCCC_FUNC___kvm_enable_ssbs,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_init_lrs,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_get_gic_config,
	__KVM_HOST_SMCCC_FUNC___kvm_flush_vm_context,
	__KVM_HOST_SMCCC_FUNC___kvm_tlb_flush_vmid_ipa,
	__KVM_HOST_SMCCC_FUNC___kvm_tlb_flush_vmid,
	__KVM_HOST_SMCCC_FUNC___kvm_flush_cpu_context,
	__KVM_HOST_SMCCC_FUNC___pkvm_prot_finalize,

	/* Hypercalls available after pKVM finalisation */
	__KVM_HOST_SMCCC_FUNC___pkvm_host_share_hyp,
	__KVM_HOST_SMCCC_FUNC___pkvm_host_unshare_hyp,
	__KVM_HOST_SMCCC_FUNC___pkvm_host_reclaim_page,
	__KVM_HOST_SMCCC_FUNC___pkvm_host_map_guest,
	__KVM_HOST_SMCCC_FUNC___kvm_adjust_pc,
	__KVM_HOST_SMCCC_FUNC___kvm_vcpu_run,
	__KVM_HOST_SMCCC_FUNC___kvm_timer_set_cntvoff,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_save_vmcr_aprs,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_restore_vmcr_aprs,
	__KVM_HOST_SMCCC_FUNC___pkvm_init_vm,
	__KVM_HOST_SMCCC_FUNC___pkvm_init_vcpu,
	__KVM_HOST_SMCCC_FUNC___pkvm_teardown_vm,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_load,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_put,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_sync_state,
};

/* <- PASTE */


/* Once again, with reflection.
 *
 * Can be a subset, but we need all the hypercallable hypercalls.
 */

#define CALL(x) { .k = #x, .v = __KVM_HOST_SMCCC_FUNC___ ## x }

struct __kv { const char *k; int v; };

static struct __kv smccc_func_numbers[] = {
	CALL(pkvm_host_share_hyp),
	CALL(pkvm_host_unshare_hyp),
	CALL(pkvm_host_reclaim_page),
	CALL(pkvm_host_map_guest),
	CALL(kvm_adjust_pc),
	CALL(kvm_vcpu_run),
	CALL(kvm_timer_set_cntvoff),
	CALL(vgic_v3_save_vmcr_aprs),
	CALL(vgic_v3_restore_vmcr_aprs),
	CALL(pkvm_init_vm),
	CALL(pkvm_init_vcpu),
	CALL(pkvm_teardown_vm),
	CALL(pkvm_vcpu_load),
	CALL(pkvm_vcpu_put),
	CALL(pkvm_vcpu_sync_state),
        { .k = 0, .v = -1 }
};
