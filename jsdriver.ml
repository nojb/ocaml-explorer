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
  class type lineCh =
    object
      method line: int Js.readonly_prop
      method ch: int Js.optdef Js.readonly_prop
    end

  val createLineCh: int -> int option -> lineCh Js.t

  class type markOptions =
    object
      method className: string Js.readonly_prop
    end

  val createMarkOptions: string -> markOptions Js.t

  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
      method getCursor: lineCh Js.t Js.meth
      method on: string -> (unit -> unit) Js.callback -> unit Js.meth
      method off: string -> (unit -> unit) Js.callback -> unit Js.meth
      method markText: lineCh Js.t -> lineCh Js.t -> markOptions Js.t -> unit Js.meth
    end

  val createCodeMirror: ?lineNumbers:bool -> ?readOnly:bool -> ?mode:string -> ?value:string -> #Dom.node Js.t -> codeMirror Js.t
end = struct
  class type lineCh =
    object
      method line: int Js.readonly_prop
      method ch: int Js.optdef Js.readonly_prop
    end

  let createLineCh line ch =
    let open Js.Unsafe in
    let ch = match ch with None -> inject Js.null | Some ch -> inject ch in
    obj [|"line", inject line; "ch", ch|]

  class type markOptions =
    object
      method className: string Js.readonly_prop
    end

  let createMarkOptions className =
    object%js
      val className = className
    end

  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
      method getCursor: lineCh Js.t Js.meth
      method on: string -> (unit -> unit) Js.callback -> unit Js.meth
      method off: string -> (unit -> unit) Js.callback -> unit Js.meth
      method markText: lineCh Js.t -> lineCh Js.t -> markOptions Js.t -> unit Js.meth
    end

  let createCodeMirror ?lineNumbers ?readOnly ?mode ?value node =
    let open Js.Unsafe in
    let add s x params = match x with Some x -> (s, inject x) :: params | None -> params in
    let params = add "lineNumbers" lineNumbers [] in
    let readOnly = match readOnly with None -> None | Some readOnly -> Some (Js.bool readOnly) in
    let params = add "readOnly" readOnly params in
    let mode = match mode with None -> None | Some mode -> Some (Js.string mode) in
    let params = add "mode" mode params in
    let value = match value with None -> None | Some value -> Some (Js.string value) in
    let params = add "value" value params in
    let node = Js.Unsafe.inject node in
    let args = if params <> [] then [|node; Js.Unsafe.obj (Array.of_list params)|] else [|node|] in
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror") args

  (* let markText cm start stop className = *)
  (*   let open Js.Unsafe in *)
  (*   let start = obj [|"line", inject start#line; "ch", inject start#ch|] in *)
  (*   let stop = obj [|"line", inject stop#line; "ch", inject stop#ch|] in *)
  (*   let className = "className", inject (Js.string className) in *)
  (*   meth_call cm "markText" [|start; stop; obj [|className|]|] *)
end

let value =
  "type t = Add of t * t | Div of t * t | Int of int

let rec eval = function
  | Add (e1, e2) -> eval e1 + eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Int n -> n

let () =
  Printf.printf \"RESULT = %d\\n%!\" (eval (Add (Int 17, Div (Int 19, Int 3))))
"

let () =
  (* Dom_html.window##alert (Js.string "hello, world"); *)
  let input_elt = Dom_html.getElementById "left" in
  let input_elt = CodeMirror.createCodeMirror ~lineNumbers:true ~mode:"text/x-ocaml" ~value input_elt in
  let output_elt = Dom_html.getElementById "right" in
  let output_elt = CodeMirror.createCodeMirror ~lineNumbers:true ~mode:"text/x-gas" ~readOnly:true output_elt in
  let cursorActivity () =
    let curs = output_elt##getCursor in
    let ch =
      match Js.Optdef.to_option curs ##. ch with
      | None -> "null"
      | Some ch -> string_of_int ch
    in
    Printf.ksprintf prerr_endline "cursorActivity (line=%d, ch=%s)" curs##.line ch;
    output_elt##markText curs (CodeMirror.createLineCh curs##.line None) (CodeMirror.createMarkOptions "foo")
  in
  output_elt##on "cursorActivity" (Js.wrap_callback cursorActivity);
  let compile_btn = Dom_html.getElementById "compile_btn" in
  let onclick _ =
    compile output_elt input_elt##getValue;
    Js.bool false
  in
  compile_btn##.onclick := Dom.handler onclick
