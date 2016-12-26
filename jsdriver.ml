let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_iter f = function
  | None -> ()
  | Some x -> f x

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
  Clflags.dlcode := false;
  Clflags.pic_code := false

let last_asm = ref []

let syntax : [`Masm | `Gas] ref = ref `Masm

let compile asmcm s =
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
  let b = Buffer.create 10000 in
  let k = ref (-1) in
  let refresh () =
    k := -1;
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
        | _ -> ()) !last_asm
  in
  let handler asm =
    last_asm := asm;
    refresh ();
  in
  Emitaux.asm_handler := Some handler;
  JsooOpt.compile "main.ml";
  let s = Buffer.contents b in
  asmcm##setValue (Js.string s)

let value =
  "type t = Add of t * t | Div of t * t | Int of int

let rec eval = function
  | Add (e1, e2) -> eval e1 + eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Int n -> n

let () =
  Printf.printf \"RESULT = %d\\n%!\" (eval (Add (Int 17, Div (Int 19, Int 3))))
"

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
  cm

let () =
  let mldiv = Dom_html.getElementById "left" in
  let mlcm = codeMirror ~mode:"text/x-ocaml" ~value mldiv in
  let asmdiv = Dom_html.getElementById "right" in
  let asmcm = codeMirror ~mode:"text/x-gas" ~readOnly:true asmdiv in
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
  mlcm##on "cursorActivity" (Js.wrap_callback cursorActivitySource);
  asmcm##on "cursorActivity" (Js.wrap_callback cursorActivity);
  let compilebut = Dom_html.getElementById "compile_btn" in
  let onclick _ =
    compile asmcm mlcm##getValue;
    Js.bool false
  in
  compilebut##.onclick := Dom.handler onclick
