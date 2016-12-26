let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

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
    method className: Js.js_string Js.t Js.readonly_prop
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
    method setSize: Js.js_string Js.t Js.opt -> Js.js_string Js.t Js.opt -> unit Js.meth
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
