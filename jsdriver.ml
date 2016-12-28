let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let console_buf : string option array = Array.make 10 None

let addconsole s =
  let rec loop i =
    if i >= Array.length console_buf then begin
      for i = 1 to Array.length console_buf - 1 do
        console_buf.(i-1) <- console_buf.(i)
      done;
      console_buf.(Array.length console_buf - 1) <- Some s
    end else begin
      match console_buf.(i) with
      | None -> console_buf.(i) <- Some s
      | Some _ -> loop (i + 1)
    end
  in
  loop 0

let getconsole () =
  let b = Buffer.create 100 in
  let rec loop i =
    if i >= Array.length console_buf then
      Buffer.contents b
    else begin
      match console_buf.(i) with
      | Some s ->
          Buffer.add_string b s;
          Buffer.add_char b '\n';
          loop (i + 1)
      | None ->
          Buffer.contents b
    end
  in
  loop 0

let log s =
  addconsole s;
  let console = Dom_html.getElementById "console" in
  console##.textContent := Js.Opt.return (Js.string (getconsole ()))

let logf fmt =
  Printf.ksprintf log fmt

let ml2asm : (int, int list) Hashtbl.t = Hashtbl.create 0
let asm2ml : (int, int) Hashtbl.t  = Hashtbl.create 0

let first = ref true

let () =
  Clflags.dlcode := false

let codeMirror ?mode ?value ?readOnly div =
  let open Dom_html in
  let cm =
    Codemirror.createCodeMirror ~lineNumbers:true ?mode ?value ?readOnly ~lineWrapping:true div
  in
  let style = window##getComputedStyle div in
  let resize _ =
    cm##setSize (Js.Opt.return style##.width) (Js.Opt.return style##.height);
    Js._false
  in
  ignore (addEventListener window Event.resize (handler resize) Js._false);
  ignore (resize ());
  cm

let value =
  "type t = Add of t * t | Div of t * t | Int of int

let rec eval = function
  | Add (e1, e2) -> eval e1 + eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Int n -> n

let () =
  Printf.printf \"RESULT = %d\\n%!\" (eval (Add (Int 17, Div (Int 19, Int 3))))
"

let mlcm =
  let mldiv = Dom_html.getElementById "left" in
  codeMirror ~mode:"text/x-ocaml" ~value mldiv

let asmcm =
  let asmdiv = Dom_html.getElementById "right" in
  codeMirror ~mode:"text/x-gas" ~readOnly:true asmdiv

let last_asm = ref []

let syntax : [`Masm | `Gas] ref = ref `Masm

let b = Buffer.create 10000

let show_directives = ref false

let refresh () =
  prerr_endline "Start refresh";
  let k = ref (-1) in
  Buffer.clear b;
  let aux (n, i) =
    incr k;
    begin match n with
    | None -> ()
    | Some n ->
        let n = n - 1 in (* zero-based *)
        Hashtbl.replace asm2ml !k n;
        let ii = try Hashtbl.find ml2asm n with Not_found -> [] in
        Hashtbl.replace ml2asm n (!k :: ii)
    end;
    let print_line =
      match !syntax with
      | `Masm -> X86_masm.print_line
      | `Gas -> X86_gas.print_line
    in
    print_line b i;
    Buffer.add_char b '\n'
  in
  List.iter (function
      | (_, (X86_ast.Ins _ | X86_ast.NewLabel _)) as arg -> aux arg
      | _ when not !show_directives -> ()
      | arg -> aux arg
    ) !last_asm;
  let s = Buffer.contents b in
  asmcm##setValue (Js.string s);
  prerr_endline "Refresh OK"

let compile s =
  Hashtbl.clear ml2asm;
  Hashtbl.clear asm2ml;
  let name = "main.ml" in
  let content = Js.to_string s in
  let f =
    if !first then
      (first := false; Sys_js.register_file)
    else
      Sys_js.update_file
  in
  f ~name ~content;
  let handler asm =
    last_asm := asm;
    refresh ();
  in
  Emitaux.asm_handler := Some handler;
  JsooOpt.compile "main.ml"

let picinput =
  Js.coerce (Dom_html.getElementById "pic") Dom_html.CoerceTo.input (fun _ -> assert false)

let () =
  let upd () = Clflags.pic_code := Js.to_bool picinput##.checked in
  let h _ =
    upd ();
    compile mlcm##getValue;
    Js._true
  in
  upd ();
  picinput##.onchange := Dom_html.handler h

let flambdainput =
  Js.coerce (Dom_html.getElementById "flambda") Dom_html.CoerceTo.input (fun _ -> assert false)

let () =
  let upd () = Config.flambda := Js.to_bool flambdainput##.checked in
  let h _ =
    upd ();
    compile mlcm##getValue;
    Js._true
  in
  upd ();
  flambdainput##.onchange := Dom_html.handler h

let directivesinput =
  Js.coerce (Dom_html.getElementById "directives") Dom_html.CoerceTo.input (fun _ -> assert false)

let () =
  let upd () = show_directives := Js.to_bool directivesinput##.checked in
  let h _ =
    upd ();
    refresh ();
    Js._true
  in
  upd ();
  directivesinput##.onchange := Dom_html.handler h

let selectsyntax =
  Js.coerce (Dom_html.getElementById "syntax") Dom_html.CoerceTo.select (fun _ -> assert false)

let () =
  let upd () =
    let r =
      match Js.to_string selectsyntax##.value with
      | "masm" -> `Masm
      | "gas" -> `Gas
      | _ -> assert false
    in
    syntax := r
  in
  let h _  =
    upd ();
    refresh ();
    Js._true
  in
  upd ();
  selectsyntax##.onchange := Dom_html.handler h

let () =
  let lastMarks = ref [] in
  let clearMarks () = List.iter (fun mark -> mark#clear) !lastMarks in
  let cursorActivitySource () =
    clearMarks ();
    let n = mlcm##getCursor##.line in
    let lines = try Hashtbl.find ml2asm n with Not_found -> [] in
    lastMarks :=
      Codemirror.addLineBackgroundClass mlcm n "foo" ::
      List.map (fun n -> Codemirror.addLineBackgroundClass asmcm n "foo") lines;
    if lines <> [] then
      let pos =
        object%js
          val from = Codemirror.createLineCh (List.fold_left min max_int lines) (Js.Opt.return 0)
          val _to = Codemirror.createLineCh (List.fold_left max 0 lines) Js.null
        end
      in
      asmcm##scrollIntoView pos
  in
  let cursorActivity () =
    clearMarks ();
    let line = asmcm##getCursor##.line in
    match Hashtbl.find asm2ml line with
    | n ->
        let lines = try Hashtbl.find ml2asm n with Not_found -> [] in
        lastMarks :=
          Codemirror.addLineBackgroundClass mlcm n "foo" ::
          List.map (fun n -> Codemirror.addLineBackgroundClass asmcm n "foo") lines;
    | exception Not_found ->
        ()
  in
  let last_timeout_id = ref None in
  let eps = 1. (* seconds *) in
  let trycompile () =
    last_timeout_id := None;
    compile mlcm##getValue
  in
  let changes _ _ =
    begin match !last_timeout_id with
    | None -> ()
    | Some id -> Dom_html.window##clearTimeout id
    end;
    last_timeout_id :=
      Some (Dom_html.window##setTimeout (Js.wrap_callback trycompile) (eps *. 1000.))
  in
  mlcm##on "cursorActivity" (Js.wrap_callback cursorActivitySource);
  mlcm##on "changes" (Js.wrap_callback changes);
  asmcm##on "cursorActivity" (Js.wrap_callback cursorActivity);
  compile mlcm##getValue
