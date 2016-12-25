let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let log s =
  let console = Dom_html.getElementById "console" in
  console##.textContent := Js.Opt.return (Js.string s)

let logf fmt =
  Printf.ksprintf log fmt

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
        let n = n - 1 in (* zero-based *)
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
      method line: int Js.prop
      method ch: int Js.opt Js.prop
    end

  val createLineCh: int -> int Js.opt -> lineCh Js.t

  class type markOptions =
    object
      method className: string Js.readonly_prop
    end

  val createMarkOptions: ?className:string -> unit -> markOptions Js.t

  class type textMarker =
    object
      method clear: unit Js.meth
    end

  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
      method getCursor: lineCh Js.t Js.meth
      method on: string -> (unit -> unit) Js.callback -> unit Js.meth
      method off: string -> (unit -> unit) Js.callback -> unit Js.meth
      method markText: lineCh Js.t -> lineCh Js.t -> markOptions Js.t -> textMarker Js.t Js.meth
      method scrollIntoView: _ Js.t -> unit Js.meth
      method addLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
      method removeLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    end

  val createCodeMirror: ?lineNumbers:bool -> ?readOnly:bool -> ?mode:string -> ?value:string -> ?lineWrapping:bool -> #Dom.node Js.t -> codeMirror Js.t
end = struct
  class type lineCh =
    object
      method line: int Js.prop
      method ch: int Js.opt Js.prop
    end

  let createLineCh line ch =
    object%js
      val mutable line = line
      val mutable ch = ch
    end

  class type markOptions =
    object
      method className: string Js.readonly_prop
    end

  let createMarkOptions ?className () =
    let open Js.Unsafe in
    let add s x l = match x with None -> l | Some x -> (s, inject x) :: l in
    let fields = [] in
    let fields = add "className" (opt_map Js.string className) fields in
    obj (Array.of_list fields)

  class type textMarker =
    object
      method clear: unit Js.meth
    end

  class type codeMirror =
    object
      method getValue: Js.js_string Js.t Js.meth
      method setValue: Js.js_string Js.t -> unit Js.meth
      method getCursor: lineCh Js.t Js.meth
      method on: string -> (unit -> unit) Js.callback -> unit Js.meth
      method off: string -> (unit -> unit) Js.callback -> unit Js.meth
      method markText: lineCh Js.t -> lineCh Js.t -> markOptions Js.t -> textMarker Js.t Js.meth
      method scrollIntoView: _ Js.t -> unit Js.meth
      method addLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
      method removeLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    end

  let createCodeMirror ?lineNumbers ?readOnly ?mode ?value ?lineWrapping node =
    let open Js.Unsafe in
    let add s x params = match x with Some x -> (s, inject x) :: params | None -> params in
    let params = add "lineNumbers" lineNumbers [] in
    let params = add "readOnly" readOnly params in
    let params = add "mode" (opt_map Js.string mode) params in
    let params = add "value" (opt_map Js.string value) params in
    let params = add "lineWrapping" lineWrapping params in
    let node = inject node in
    let args = if params <> [] then [|node; obj (Array.of_list params)|] else [|node|] in
    fun_call (js_expr "CodeMirror") args
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
  let input_elt = CodeMirror.createCodeMirror ~lineNumbers:true ~mode:"text/x-ocaml" ~value ~lineWrapping:true input_elt in
  let output_elt = Dom_html.getElementById "right" in
  let output_elt = CodeMirror.createCodeMirror ~lineNumbers:true ~mode:"text/x-gas" ~readOnly:true output_elt in
  let lastMarks = ref [] in
  let lastMarksSource = ref None in
  let cursorActivitySource () =
    List.iter (fun line -> output_elt##removeLineClass line (Js.string "background") (Js.string "foo")) !lastMarks;
    opt_iter (fun line -> input_elt##removeLineClass line (Js.string "background") (Js.string "foo")) !lastMarksSource;
    let curs = input_elt##getCursor in
    let ch =
      match Js.Opt.to_option curs##.ch with
      | None -> "null"
      | Some ch -> string_of_int ch
    in
    Printf.ksprintf prerr_endline "cursorActivitySource (line=%d, ch=%s)" curs##.line ch;
    let n = curs##.line in
    logf "LINE: %d" n;
    let lines = try Hashtbl.find src2asm n with Not_found -> [] in
    lastMarks := lines;
    List.iter (fun line ->
        output_elt##addLineClass line (Js.string "background") (Js.string "foo")
      ) lines;
    lastMarksSource := Some n;
    input_elt##addLineClass n (Js.string "background") (Js.string "foo");
    if lines <> [] then
      let pos =
        object%js
          val from =
            object%js
              val line = List.fold_left min max_int lines
              val ch = Js.Opt.return 0
            end
          val _to =
            object%js
              val line = List.fold_left max 0 lines
              val ch = Js.null
            end
        end
      in
      output_elt##scrollIntoView pos
  in
  let cursorActivity () =
    List.iter (fun line -> output_elt##removeLineClass line (Js.string "background") (Js.string "foo")) !lastMarks;
    opt_iter (fun line -> input_elt##removeLineClass line (Js.string "background") (Js.string "foo")) !lastMarksSource;
    let curs = output_elt##getCursor in
    let ch =
      match Js.Opt.to_option curs##.ch with
      | None -> "null"
      | Some ch -> string_of_int ch
    in
    Printf.ksprintf prerr_endline "cursorActivity (line=%d, ch=%s)" curs##.line ch;
    let line = curs##.line in
    logf "LINE: %d" line;
    match Hashtbl.find asm2src line with
    | n ->
        let lines = try Hashtbl.find src2asm n with Not_found -> [] in
        lastMarks := lines;
        List.iter (fun line ->
            output_elt##addLineClass line (Js.string "background") (Js.string "foo")
          ) lines;
        lastMarksSource := Some n;
        input_elt##addLineClass n (Js.string "background") (Js.string "foo");
    | exception Not_found ->
        ()
  in
  input_elt##on "cursorActivity" (Js.wrap_callback cursorActivitySource);
  output_elt##on "cursorActivity" (Js.wrap_callback cursorActivity);
  let compile_btn = Dom_html.getElementById "compile_btn" in
  let onclick _ =
    compile output_elt input_elt##getValue;
    Js.bool false
  in
  compile_btn##.onclick := Dom.handler onclick
