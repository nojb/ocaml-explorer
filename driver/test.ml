let unoption opt =
  match Js.Opt.to_option opt with
  | Some x -> x
  | None -> assert false

let compile output_elt s =
  Sys_js.register_file ~name:"main.ml" ~content:(Js.to_string s);
  Optmain.compile "main.ml"
  (* output_elt##.value := s *)

let () =
  (* Dom_html.window##alert (Js.string "hello, world"); *)
  let output_elt = Dom_html.getElementById "output_elt" in
  let output_elt = unoption (Dom_html.CoerceTo.textarea output_elt) in
  let input_elt = Dom_html.getElementById "input_elt" in
  let input_elt = unoption (Dom_html.CoerceTo.textarea input_elt) in
  (* let onkeydown _ = *)
  (*   prerr_endline "keyDown!"; *)
  (*   Js.bool true *)
  (* in *)
  (* input_elt##.onkeydown := Dom.handler onkeydown; *)
  let compile_btn = Dom_html.getElementById "compile_btn" in
  let onclick _ =
    compile output_elt input_elt##.value;
    Js.bool false
  in
  compile_btn##.onclick := Dom.handler onclick;
