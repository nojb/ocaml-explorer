class type lineCh =
  object
    method line: int Js.prop
    method ch: int Js.opt Js.prop
  end

val createLineCh: int -> int Js.opt -> lineCh Js.t

class type markOptions =
  object
    method className: Js.js_string Js.t Js.readonly_prop
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
    method on: string -> _ Js.callback -> unit Js.meth
    method off: string -> _ Js.callback -> unit Js.meth
    method markText: lineCh Js.t -> lineCh Js.t -> markOptions Js.t -> textMarker Js.t Js.meth
    method scrollIntoView: _ Js.t -> unit Js.meth
    method addLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    method removeLineClass: int -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    method setSize: Js.js_string Js.t Js.opt -> Js.js_string Js.t Js.opt -> unit Js.meth
  end

val createCodeMirror: ?lineNumbers:bool -> ?readOnly:bool -> ?mode:string -> ?value:string -> ?lineWrapping:bool -> #Dom.node Js.t -> codeMirror Js.t

val addLineBackgroundClass: codeMirror Js.t -> int -> string -> < clear: unit >
