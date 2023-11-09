#include <sys/types.h>
#include <linux/types.h>
#include <linux/kvm.h>
#include <sys/sysinfo.h>


/* XXX
 * This comes from linux/kvm.h, but only for aarch64. Mirrored here simply
 * to make it compile for x86_64.
 */
#ifdef __x86_64__
struct user_pt_regs {
        __u64           regs[31];
        __u64           sp;
        __u64           pc;
        __u64           pstate;
};
#endif

#define __KVM_HOST_SMCCC_FUNC___kvm_hyp_init			0

enum __kvm_host_smccc_func {
	/* Hypercalls available only prior to pKVM finalisation */
	/* __KVM_HOST_SMCCC_FUNC___kvm_hyp_init */
	__KVM_HOST_SMCCC_FUNC___kvm_get_mdcr_el2 =
		__KVM_HOST_SMCCC_FUNC___kvm_hyp_init + 1,
	__KVM_HOST_SMCCC_FUNC___pkvm_init,
	__KVM_HOST_SMCCC_FUNC___pkvm_create_private_mapping,
	__KVM_HOST_SMCCC_FUNC___pkvm_cpu_set_vector,
	__KVM_HOST_SMCCC_FUNC___kvm_enable_ssbs,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_init_lrs,
	__KVM_HOST_SMCCC_FUNC___vgic_v3_get_gic_config,
	__KVM_HOST_SMCCC_FUNC___kvm_flush_vm_context,
	__KVM_HOST_SMCCC_FUNC___kvm_tlb_flush_vmid_ipa,
	__KVM_HOST_SMCCC_FUNC___kvm_tlb_flush_vmid, /* 10 */
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
	__KVM_HOST_SMCCC_FUNC___vgic_v3_save_vmcr_aprs, /* 20 */
	__KVM_HOST_SMCCC_FUNC___vgic_v3_restore_vmcr_aprs,
	__KVM_HOST_SMCCC_FUNC___pkvm_init_vm,
	__KVM_HOST_SMCCC_FUNC___pkvm_init_vcpu,
	__KVM_HOST_SMCCC_FUNC___pkvm_teardown_vm,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_load,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_put,
	__KVM_HOST_SMCCC_FUNC___pkvm_vcpu_sync_state,
};

#define HPROX_HVC_TYPE 'h'
#define HPROX_STRUCTS_TYPE 's'
#define HPROX_ALLOC_TYPE 'a'
#define HPROX_MEMCACHE_TYPE 'm'


// Perform the HVC numbered hvcnum, with this number of arguments.
// The ioctl parameter is an array containing the arguments
#define HVC_PROXY_IOCTL(hvcnum, numarg) \
	_IOC(_IOC_WRITE, HPROX_HVC_TYPE, hvcnum, 8 * numarg)

// All those ioctl return a size or an offset as return value.
#define HPROX_STRUCT_KVM_GET_SIZE _IO(HPROX_STRUCTS_TYPE, 0)
// The argument must be a: enum struct_kvm_fields
#define HPROX_STRUCT_KVM_GET_OFFSET _IO(HPROX_STRUCTS_TYPE, 1)
#define HPROX_HYP_VM_GET_SIZE _IO(HPROX_STRUCTS_TYPE, 2)
#define HPROX_PGD_GET_SIZE _IO(HPROX_STRUCTS_TYPE, 3)
#define HPROX_STRUCT_KVM_VCPU_GET_SIZE _IO(HPROX_STRUCTS_TYPE, 4)
// The argument must be a: enum struct_kvm_vcpu_fields
#define HPROX_STRUCT_KVM_VCPU_GET_OFFSET _IO(HPROX_STRUCTS_TYPE, 5)
#define HPROX_HYP_VCPU_GET_SIZE _IO(HPROX_STRUCTS_TYPE, 6)

enum struct_kvm_fields {
	HPROX_NR_MEM_SLOT_PAGES, /* unsigned long */
	HPROX_VCPU_ARRAY, /* xarray */
	HPROX_MAX_VCPUS, /* int */
	HPROX_CREATED_VCPUS, /* int */
	HPROX_ARCH_PKVM_ENABLED, /* bool */
	HPROX_ARCH_PKVM_TEARDOWN_MC, /* struct hprox_memcache */
};

enum struct_kvm_vcpu_fields {
	HPROX_VCPU_ID, /* int */
	HPROX_VCPU_IDX, /* int */
	HPROX_VCPU_CFLAGS, /* 8 bits bitfield */
	HPROX_VCPU_IFLAGS, /* 8 bits bitfield */
	HPROX_VCPU_FEATURES, /* KVM_VCPU_MAX_FEATURES bits bitfield */
	HPROX_VCPU_HCR_EL2, /* u64 */
	HPROX_VCPU_FAULT, /* struct hprox_vcpu_fault_info */
	HPROX_VCPU_REGS, /* struct user_pt_regs */
	HPROX_VCPU_FP_REGS, /* struct user_fpsimd_state */
	HPROX_VCPU_MEMCACHE, /* struct hprox_memcache */
	// TODO add SVE state, for now SVE-less guests only
};

struct hprox_vcpu_fault_info {
	__u64 esr_el2; /* Hyp Syndrom Register */
	__u64 far_el2; /* Hyp Fault Address Register */
	__u64 hpfar_el2; /* Hyp IPA Fault Address Register */
	__u64 disr_el1; /* Deferred [SError] Status Register */
};

// Need to match up kvm_hyp_memcache
struct hprox_memcache {
        __u64 head; // kernel address, might not be accessible, if not
			    // donated from a hprox_alloc region.
	unsigned long nr_pages;
};
enum hprox_alloc_type { HPROX_VMALLOC, HPROX_PAGES_EXACT };

// the ioctl parameter is the size of the allocation
#define HPROX_ALLOC(alloc) _IO(HPROX_ALLOC_TYPE, alloc)
#define HPROX_ALLOC_PAGES HPROX_ALLOC(HPROX_PAGES_EXACT)

// ioctl on the mmapable fd from the HPROX_ALLOC ioctl
#define HPROX_ALLOC_KADDR _IOR('A',0, __u64)
#define HPROX_ALLOC_PHYS _IOR('A', 1, __u64)
#define HPROX_ALLOC_RELEASE _IO('A', 2)
#define HPROX_ALLOC_FREE _IO('A', 3)

// memcache ioctl, free is encoded as topup 0
#define HPROX_MEMCACHE_FREE \
	_IOWR(HPROX_MEMCACHE_TYPE, 0, struct hprox_memcache)
#define HPROX_MEMCACHE_TOPUP(n) \
	_IOWR(HPROX_MEMCACHE_TYPE, (n), struct hprox_memcache)
