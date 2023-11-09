open Pkvm_proxy
module Log = (val Logs.(Src.create "kvmtest" |> src_log))

let _ =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let _ =
  sched_setaffinity [|0|]; (* vcpu_load .. vcpu_put *)

  let vm, handle = init_vm ~protected:true () in
  let vcpu = init_vcpu handle 0 in
  vcpu.@[vcpu_regs] <- { regs = [| 0x37L; 0x1300L; 0xdeadL |]; pc = 0x1000L; sp = 0L; pstate = 0L };
  vcpu_set_dirty vcpu;

  topup_hyp_memcache vcpu.@[vcpu_memcache] 10;

  vcpu_load handle 0;

  let gcode = kernel_region_alloc page_size in
  region_memory gcode |> Bigstring.blit_from_string "\x20\x00\x00\x8b\x40\x00\x00\xf9\x00\x00\x20\xd4";
  kernel_region_release gcode;
  map_region_guest vcpu gcode vcpu.@[vcpu_regs].pc;

  let exit_code = vcpu_run vcpu in
  Log.app (fun k -> k "@[<2>Ran VCPU 0,@ exit %a,@ fault %a@]"
      pp_arm_exception exit_code pp_fault_info vcpu.@[vcpu_fault]);
  vcpu_sync_state ();
  Log.app (fun k -> k "%a" pp_regs vcpu.@[vcpu_regs]);

  vcpu_put ();
  teardown_vm handle vm;

  kernel_region_unshare_hyp vcpu;
  kernel_region_free vcpu;

  kernel_region_reclaim gcode;
  kernel_region_free gcode;

  Gc.full_major(); (* see if it crashes *)

  ()
