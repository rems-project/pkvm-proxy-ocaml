open Ppxlib

let mktemp =
  let open Unix in
  let n = ref 0 in
  let tmp = try getenv "TMPDIR" with Not_found -> "/tmp" in
  let rec go () =
    incr n;
    let f = Fmt.str "%s/ppx-asm-%06d-%d.tmp" tmp (getpid ()) !n in
    match Unix.(openfile f [O_CREAT; O_EXCL] 0o600) with
    | exception Unix_error _ -> go ()
    | fd -> Unix.close fd; f
  in go
let rm f = try Unix.unlink f with Unix.Unix_error (Unix.ENOENT, _, _ ) -> ()
let input_eof ic =
  let rec go buf = match Buffer.add_channel buf ic 4096 with
  | exception End_of_file -> Buffer.contents buf
  | () -> go buf in
  go (Buffer.create 17)
let readfile fn =
  let ic = open_in fn in
  Fun.protect (fun () -> input_eof ic) ~finally:(fun () -> close_in ic)
 
let error_fmt fmt = Fmt.kstr (fun s -> Error s) fmt

let assemble ~gas ~objcopy prg =
  let f_elf, f_err, f_obj = mktemp (), mktemp (), mktemp () in
  let finally () = rm f_elf; rm f_err; rm f_obj in
  Fun.protect ~finally @@ fun () ->
    let oc = Unix.open_process_out (Fmt.str "%s -o %s 2>%s" gas f_elf f_err) in
    output_string oc prg; output_string oc "\n";
    match Unix.close_process_out oc with
    | Unix.WEXITED 0 ->
        let oc = Unix.open_process_out (Fmt.str "%s -O binary %s %s" objcopy f_elf f_obj) in
        ( match Unix.close_process_out oc with
          | Unix.WEXITED 0 -> Ok (readfile f_obj)
          | _ -> error_fmt "%s: exited with error" objcopy )
    | _ ->
        let err = readfile f_err in
        let err = Str.(global_replace (regexp_string "{standard input}:") "" err) in
        error_fmt "%s:\n%s" gas err

let prefix = ref ""

let expand ~ctxt prg =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let gas     = !prefix ^ "as"
  and objcopy = !prefix ^ "objcopy" in
  match assemble ~gas ~objcopy prg with
  | Ok bin -> Ast_builder.Default.estring ~loc bin
  | Error msg -> Location.error_extensionf ~loc "%s" msg
                  |> Ast_builder.Default.pexp_extension ~loc

let ext =
  Extension.V3.declare "asm"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let () =
  Driver.add_arg "--with-tc-prefix" (Arg.String (fun x -> prefix := x))
    ~doc:"<command-prefix> (Cross-) toolchain prefix for [%asm] extension nodes";
  Driver.register_transformation "ppx_asm"
    ~rules:[Ppxlib.Context_free.Rule.extension ext]
