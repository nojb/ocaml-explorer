let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let console =
  Dom_html.getElementById "console"

let log s =
  let s =
    match Js.Opt.to_option console##.textContent with
    | None -> s
    | Some s0 -> Js.to_string s0 ^ s
  in
  console##.textContent := Js.Opt.return (Js.string s)

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

let syntax : [`Masm | `Gas] ref = ref `Gas

let b = Buffer.create 10000

let show_directives = ref true

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
  let b = Buffer.create 0 in
  let ppf = Format.formatter_of_buffer b in
  match JsooOpt.compile ppf "main.ml" with
  | Ok () ->
      log "All OK.\n"
  | Error () ->
      Format.pp_print_flush ppf ();
      log (Buffer.contents b)

let picinput =
  Js.coerce (Dom_html.getElementById "pic") Dom_html.CoerceTo.input (fun _ -> assert false)

let () =
  let upd () = Clflags.pic_code := Js.to_bool picinput##.checked in
  let h _ =
    upd ();
    compile mlcm##getValue;
    Js._true
  in
  picinput##.checked := Js.bool !Clflags.pic_code;
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
  flambdainput##.checked := Js.bool !Config.flambda;
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
  directivesinput##.checked := Js.bool !show_directives;
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
  selectsyntax##.value := Js.string (match !syntax with `Masm -> "masm" | `Gas -> "gas");
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
