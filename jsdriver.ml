let unoption opt =
  match Js.Opt.to_option opt with
  | Some x -> x
  | None -> assert false

let src2asm : (int, int list) Hashtbl.t = Hashtbl.create 0
let asm2src : (int, int) Hashtbl.t  = Hashtbl.create 0

let first = ref true

let compile output_elt s =
  Hashtbl.clear src2asm;
  Hashtbl.clear asm2src;
  let name = "main.ml" in
  let content = Js.to_string s in
  let f =
    if !first then
      (first := false; Sys_js.register_file)
    else
      Sys_js.update_file
  in
  f ~name ~content;
  let b = Buffer.create 10000 in
  let i = ref 0 in
  let out n l =
    incr i;
    begin match n with
    | None -> ()
    | Some n ->
        Hashtbl.replace asm2src !i n;
        let ii = try Hashtbl.find src2asm n with Not_found -> [] in
        Hashtbl.replace src2asm n (!i :: ii)
    end;
    Buffer.add_string b l
  in
  X86_gas.asm_line_callback := Some out;
  Optmain.compile "main.ml";
  let s = Buffer.contents b in
  output_elt##setValue (Js.string s)

module CodeMirror : sig
  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
    end

  val createCodeMirror: ?lineNumbers:bool -> ?readOnly:bool -> ?mode:string -> #Dom.node Js.t -> codeMirror Js.t
end = struct
  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
    end

  let createCodeMirror ?lineNumbers ?readOnly ?mode node =
    let readOnly = match readOnly with Some true -> Some "nofucus" | Some false -> Some "false" | None -> None in
    let add s x params = match x with Some x -> (s, Js.Unsafe.inject x) :: params | None -> params in
    let params = add "lineNumbers" lineNumbers [] in
    let params = add "readOnly" readOnly params in
    let params = add "mode" mode params in
    let node = Js.Unsafe.inject node in
    let args = if params <> [] then [|node; Js.Unsafe.obj (Array.of_list params)|] else [|node|] in
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror") args
end

let () =
  (* Dom_html.window##alert (Js.string "hello, world"); *)
  let input_elt = Dom_html.getElementById "left" in
  let input_elt = CodeMirror.createCodeMirror ~lineNumbers:true input_elt in
  let output_elt = Dom_html.getElementById "right" in
  let output_elt = CodeMirror.createCodeMirror ~lineNumbers:true output_elt in
  let compile_btn = Dom_html.getElementById "compile_btn" in
  let onclick _ =
    compile output_elt input_elt##getValue;
    Js.bool false
  in
  compile_btn##.onclick := Dom.handler onclick
