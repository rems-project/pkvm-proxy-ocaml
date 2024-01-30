open Pkvm_proxy_utils

type hvc = 
| VERSION_FUNC_ID
| VENDOR_HYP_CALL_UID_FUNC_ID
| VENDOR_HYP_KVM_FEATURES_FUNC_ID
| VENDOR_HYP_KVM_HYP_MEMINFO_FUNC_ID of int64 * int64 * int64
| VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID of int64 * int64 * int64
| VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID of int64 * int64 * int64

let hvc_to_int = function
| VERSION_FUNC_ID -> 0x80000000L
| VENDOR_HYP_CALL_UID_FUNC_ID -> 0x8600ff01L
| VENDOR_HYP_KVM_FEATURES_FUNC_ID -> 0x86000000L
| VENDOR_HYP_KVM_HYP_MEMINFO_FUNC_ID _ -> 0xc6000002L
| VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID _ -> 0xc6000003L
| VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID _ -> 0xc6000004L

type reg = X of int | W of int
type instr =
| Hvc  of int * hvc
| Quot of string
| Mov  of reg * int64
| Movk of reg * int64 * int
| Movz of reg * int64 * int
| Str  of reg * reg
| Ldr  of reg * reg

let x n = X n and w n = W n
let hvc imm fn = Hvc (imm, fn)
and quot fmt = Fmt.kstr (fun s -> Quot s) fmt
and mov r imm = Mov (r, imm)
and str r1 r2 = Str (r1, r2)
and ldr r1 r2 = Ldr (r1, r2)

let pp_r ppf = function X n -> Fmt.pf ppf "x%d" n | W n -> Fmt.pf ppf "w%d" n
let pp_shf ppf = function 0 -> () | n -> Fmt.pf ppf ", lsl %d" n

let rec lower = function
| Hvc (imm, fn)::is ->
    let params = match fn with
    | VERSION_FUNC_ID
    | VENDOR_HYP_CALL_UID_FUNC_ID
    | VENDOR_HYP_KVM_FEATURES_FUNC_ID -> []
    | VENDOR_HYP_KVM_HYP_MEMINFO_FUNC_ID (v1, v2, v3)
    | VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID (v1, v2, v3)
    | VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID (v1, v2, v3) ->
        [Mov (W 1, v1); Mov (W 2, v2); Mov (W 3, v3)]
    in
    lower (params @ Mov (W 0, hvc_to_int fn) :: quot "hvc %d" imm :: is)
| Mov (r, imm)::is when imm <= 0xffffL ->
    Fmt.str "mov %a, 0x%04Lx" pp_r r imm :: lower is
| Mov (r, imm)::is when imm <= 0xffffffffL ->
    let open Int64 in
    lower (Movz (r, imm land 0xffffL, 0) :: Movk (r, imm lsr 16, 16) :: is)
| Mov (r, imm):: _ ->
    Fmt.invalid_arg "Mov (%a, 0x%Lx) - immediate too large (fixme)" pp_r r imm
| Str (r1, r2) :: is -> Fmt.str "str %a, [%a]" pp_r r1 pp_r r2 :: lower is
| Ldr (r1, r2) :: is -> Fmt.str "ldr %a, [%a]" pp_r r1 pp_r r2 :: lower is
| Movk (r, imm, shf) :: is -> Fmt.str "movk %a, 0x%04Lx%a" pp_r r imm pp_shf shf :: lower is
| Movz (r, imm, shf) :: is -> Fmt.str "movz %a, 0x%04Lx%a" pp_r r imm pp_shf shf :: lower is
| Quot s :: is -> s :: lower is
| [] -> []

let pp_prog = Fmt.(using lower (list ~sep:cut string |> vbox))

module Log = (val log)

let keystone = Keystone.(create ARM64 [])

let asm prog =
  Log.debug (fun k -> k "guest program:@,@[%a@]" pp_prog prog);
  match Keystone.asm keystone (Fmt.str "%a" pp_prog prog) with
  | Ok (code, _) -> code
  | Error (`Msg msg) -> Fmt.invalid_arg "asm: %s" msg
