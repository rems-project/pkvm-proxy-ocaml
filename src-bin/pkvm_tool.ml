open Pkvm_proxy
open Pkvm_proxy_utils

module Log = (val Logs.(Src.create "kvmtest" |> src_log))

let def_regs   = [ 0x37L; 0x1300L; 0xdeadL ]
let def_pc     = 0x0L
let def_sp     = 0x0L
let def_pstate = 0x0L

let page_size = 0x1000

let code pg = Pkvm_asm.(asm [
  mov (x 0) 0xfecabebaL;
  mov (x 1) 0x3713dec0L;
  mov (x 2) 0xfecabebaL;
  mov (x 3) 0x3713dec0L;

  hvc 0 VERSION_FUNC_ID;
  hvc 0 (VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID (pg, 0L, 0L));
  hvc 0 (VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID (pg, 0L, 0L));

  mov (x 26) 0xcaffL; mov (x 27) 0xff00L; str (x 26) (x 27);
])

let main ?(protected = false) ?(iters = 1) ~regs ?(addr_code = 0x0L) ?(addr_pg = 0x5000L) () =

  let code = code addr_pg in

  Log.app (fun k ->
    k "START @[<v>protected: %b@ iters: %n@ \
       code at: 0x%Lx@ page at: 0x%Lx@ regs: %a@]"
    protected iters addr_code addr_pg pp_regs regs);

  sched_setaffinity [|0|]; (* vcpu_load .. vcpu_put *)

  let vm = init_vm ~protected () in

  let vcpu = init_vcpu vm 0 in
  set_vcpu_regs vcpu regs;
  topup_vcpu_memcache vcpu 10;

  vcpu_load vcpu;

  let gcode = kernel_region_alloc page_size in
  Bigstring.blit_from_string code (region_memory gcode);
  kernel_region_release gcode;
  map_region_guest vcpu.mem gcode addr_code;

  let mempg = kernel_region_alloc page_size in
  (* region_memory mempg |> ignore; *)
  kernel_region_release mempg;
  map_region_guest vcpu.mem mempg addr_pg;

  for _ = 1 to iters do
    let exit_code = vcpu_run vcpu in
    Log.app (fun k -> k "@[<2>Ran VCPU 0,@ exit %d,@ fault %a@]"
        exit_code pp_fault_info vcpu.mem.@[vcpu_fault]);
    (* Fmt.pr "mem => %a@." (Bigstring.hex ()) (Bigarray.Array1.sub (region_memory mempg) 0 32); *)
    vcpu_sync_state ();
    Log.app (fun k -> k "%a" pp_regs vcpu.mem.@[vcpu_regs]);
  done;

  vcpu_put ();

  teardown_vm vm;

  teardown_vcpu vcpu;

  kernel_region_reclaim gcode;
  kernel_region_free gcode;

  kernel_region_reclaim mempg;
  kernel_region_free mempg;

  Gc.full_major(); (* see if it crashes *)

  ()

open Cmdliner

let ($$) f a = Term.(const f $ a)

let int64x =
  let rd s =
    let open Scanf in
    match sscanf s "0x%Lx%s" (fun x r -> x, r) with
    | x, "" -> Ok x
    | _, s -> Error (`Msg s)
    | exception Scan_failure _ ->
        match sscanf s "%Ld%s" (fun x r -> x, r) with
        | x, "" -> Ok x
        | _, s -> Error (`Msg s)
        | exception Scan_failure _ -> Error (`Msg "cannot read") in
  Arg.conv (rd, fun ppf -> Fmt.pf ppf "0x%08Lx") ~docv:"hex (0xXXXX) or decimal"

let info = Cmd.info "kvmtest" ~doc:"pKVM test"

let registers =
  let open Arg in
  let regs = value @@ opt (list int64x) def_regs @@ info ["regs"] ~doc:"GP registers"
  and pc     = value @@ opt int64x def_pc     @@ info ["pc"] ~doc:"PC register"
  and sp     = value @@ opt int64x def_sp     @@ info ["sp"] ~doc:"SP register"
  and pstate = value @@ opt int64x def_pstate @@ info ["pstate"] ~doc:"PSTATE register" in
  Term.((fun regs pc sp pstate -> { regs = Array.of_list regs; pc; sp; pstate })
    $$ regs $ pc $ sp $ pstate)

let term =
  let open Arg in
  let protected = value @@ flag @@ info ["p"; "protected"] ~doc:"Protected mode"
  and iters     = value @@ opt int 1 @@ info ["i"; "iterations"] ~doc:"VCPU_RUN iterations"
  and addr_code = value @@ opt (some int64x) None @@ info ["addr-code"] ~doc:"Address of the code page"
  and addr_pg   = value @@ opt (some int64x) None @@ info ["addr-page"] ~doc:"Address of the second page"
  in
  Term.((fun () protected iters regs addr_code addr_pg ->
      main ~protected ~iters ~regs ?addr_code ?addr_pg ())
    $$ (Logs.set_level ~all:true $$ Logs_cli.level ())
    $ protected
    $ iters
    $ registers
    $ addr_code $ addr_pg
    )

let _ =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Cmd.v info term |> Cmd.eval