type reg
(** Registers **)

val x : int -> reg
val w : int -> reg
val pp_r : Format.formatter -> reg -> unit

type instr
(** Instructions **)

val mov : reg -> int64 -> instr
val str : reg -> reg -> instr
val ldr : reg -> reg -> instr

val quot : ('a, Format.formatter, unit, instr) format4 -> 'a
(** Directly format an instruction, e.g.
    [quot "movk %a, 0x%04Lx" pp_r (x 2) 0x1234L]. *)

type hvc = 
| VERSION_FUNC_ID
| VENDOR_HYP_CALL_UID_FUNC_ID
| VENDOR_HYP_KVM_FEATURES_FUNC_ID
| VENDOR_HYP_KVM_HYP_MEMINFO_FUNC_ID of int64 * int64 * int64
| VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID of int64 * int64 * int64
| VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID of int64 * int64 * int64

val hvc : int -> hvc -> instr

val asm : instr list -> string
(** [asm prg] is the encoding for the program prg. *)
