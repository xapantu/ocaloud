[%%shared
    open Eliom_content
    open Eliom_lib
    type ('a, 'b) cocaml = (unit, 'a, Eliom_service.service_method,
                      Eliom_service.attached,
                      Eliom_service.service_kind,
                      [ `WithoutSuffix ], unit, string,
                      Eliom_service.registrable,
                      'b Eliom_service.ocaml_service) Eliom_service.service
    type form_content = Html5_types.form_content_fun Eliom_content.Html5.elt
]
[%%client
    open Lwt
    let get_from_server service param = Eliom_client.call_ocaml_service ~service () param
    exception Couldnt_unwrap
    let unwrap = function
      | Some s -> s
      | None -> raise Couldnt_unwrap
]


module Form(Data: App_stub.DATA) = struct
  type (_, _) atom =
    | TInt			: (int, int) atom
    | TBool    	  	: (bool, bool) atom
    | TString   		: (string, string) atom
    | TStringPassword	: (string, string) atom
    | TStringList : ('a list React.signal * ('a -> string)) -> ('a, string) atom

  type (_, _) params_type =
    | TProd : ( ('a, 'aa) params_type * ('b, 'bb) params_type) -> ('a * 'b, 'aa * 'bb) params_type
    | TAtom : (string * 'a * ('a, 'b) atom) -> ('a, 'b) params_type
    | TAtomOpt : (string * 'a option * ('a, 'b) atom) -> ('a, 'b) params_type

  let int (n : string) def = TAtom (n,def, TInt)

  let bool (n : string) def = TAtom (n, def, TBool)

  let string (n : string) def = TAtom (n, def, TString)
  let string_password (n : string) def = TAtom (n,def, TStringPassword)

  let string_list n signal_data get_description = TAtomOpt(n, None, TStringList(signal_data, get_description))

  let prod t1 t2 = TProd (t1,t2)

  let ( ** ) = prod

  let service_stub param func =
    Eliom_registration.Ocaml.register_post_coservice'
      ~post_params:param
      (fun () p -> func p)

  let client_prod: (unit -> 'a) Eliom_lib.client_value -> (unit -> 'b) Eliom_lib.client_value -> (unit -> ('a * 'b)) Eliom_lib.client_value =
    fun a b ->
      ([%client (fun () ->
           ~%a (), ~%b ())])

  (* too much magic in that… *)
  let rec to_eliom: type a b c. (a, b) params_type -> ((b, 'd, c) Eliom_parameter.params_type * (b -> a)) = function
    | TAtom(n, _, TString) -> Obj.magic @@ (Eliom_parameter.string n, fun i -> i)
    | TAtom(n, _, TStringPassword) -> Obj.magic @@ (Eliom_parameter.string n, fun i -> i)
    | TAtom(n, _, TInt) -> Obj.magic @@ (Eliom_parameter.int n, fun i -> i)
    | TAtom(n, _, TBool) -> Obj.magic @@ (Eliom_parameter.bool n, fun i -> i)
    | TProd(a, b) ->
      let p1, t1 = to_eliom a in
      let p2, t2 = to_eliom b in
      Obj.magic @@ (Eliom_parameter.(p1 ** p2), fun (a, b) -> t1 a, t2 b)
    | TAtomOpt(n, _, TStringList(src_list, to_string)) ->
      Obj.magic @@ (Eliom_parameter.string n, (fun i ->
        List.find (fun a -> to_string a = i) (React.S.value src_list)))
    | TAtom(_, _, TStringList(_)) -> failwith "string list not mandatory"
    | TAtomOpt(_) -> failwith "unhandled optional field"

  let make: ('a, 'b) params_type -> ('a -> 'c Lwt.t) -> ('c -> unit) Eliom_lib.client_value option -> unit -> Widgets.div_content =
    fun params callback client_callback ->
      let eliom_params, (translator: 'b -> 'a) = to_eliom params in
      let coservice: ('b, 'c) cocaml = service_stub eliom_params (fun l -> callback (translator l)) in
      let rec build_form: type a b. (a, b) params_type -> form_content * ((unit -> b) Eliom_lib.client_value) = function
        | TAtom(name, default, TString) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TStringPassword) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Password; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TInt) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name; a_value (string_of_int default)] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              int_of_string @@ Js.to_string i##.value] : (unit -> int) Eliom_lib.client_value)
        | TAtom(name, default, TBool) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Checkbox; a_placeholder name; a_value (string_of_bool default)] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              bool_of_string @@ Js.to_string i##.value] : (unit -> bool) Eliom_lib.client_value)
        | TAtomOpt(name, default_opt, TStringList(src_list, src_to_string)) ->
          let down_event =
            React.S.map (List.map src_to_string) src_list
            |> Eliom_react.S.Down.of_react in
          let select = [%client
            let open Eliom_content.Html5.D in
            ~%down_event
            |> React.S.map (fun l ->
              List.map (fun a ->
                option (pcdata a)
              ) l
              |> Raw.select
            )
            |> Html5.R.node
          ]
          in
          Obj.magic (Html5.C.node select), ([%client (fun () ->
               let i = Eliom_content.Html5.To_dom.of_select ~%select in
               Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TProd(a, b) -> 
          mk_prod a b
        | TAtom(_, _, TStringList(_)) -> failwith "stringlist must be optional"
        | TAtomOpt(_) -> failwith "optional not properly handled"
      and
        (* too much magic in there, anyone got any idea for that? *)
        mk_prod: type a b c d. ((a, c) params_type -> (b, d) params_type -> form_content * ((unit -> c * d) Eliom_lib.client_value)) = fun a b ->
          let a, fa = build_form a
          and b, fb = build_form b in
          Html5.F.div [a; b], Obj.magic (client_prod (Obj.magic fa) (Obj.magic fb))
      in
      fun () ->
        let elt, (get_values:(unit -> 'b) Eliom_lib.client_value) = build_form params in
        let myinput = Html5.D.(input ~a:[a_input_type `Submit] ()) in
        let full_cb = [%client fun e ->
             Dom.preventDefault e;

             let rec disable_everything src =
               Dom_html.CoerceTo.element src
               |> fun a -> Js.Opt.map a (fun src ->
                 let src = Dom_html.CoerceTo.input src in
                 Js.Opt.map src (fun src ->
                 src##setAttribute (Js.string "disabled") (Js.string "1")));

               List.iter disable_everything (Dom.list_of_nodeList (src##.childNodes))
             in
             Js.Opt.map (Eliom_content.Html5.To_dom.of_element ~%myinput)##.parentNode disable_everything;
             let enable_everything () =
               let rec aux src = 
                 Dom_html.CoerceTo.element src
                 |> fun a -> Js.Opt.map a (fun src ->
                   let src = Dom_html.CoerceTo.input src in
                   Js.Opt.map src (fun src ->
                     src##removeAttribute (Js.string "disabled")));

                 List.iter aux (Dom.list_of_nodeList (src##.childNodes))
               in
               Js.Opt.map (Eliom_content.Html5.To_dom.of_element ~%myinput)##.parentNode aux;
             in
             let canceled =
               let%lwt () = Lwt_js.sleep 20.0 in
               let () = Js.Unsafe.eval_string "alert('Disconnected from the server')" in
               enable_everything (); Lwt.return ()
             in

             try
               (* FIXME: ok, we need to make sure this is not garbage collected too quickly,
                * unfortunately there is no Lwt_main in js *)
               let _ =
                 let%lwt res = get_from_server ~%coservice (~%get_values ()) in
                 Lwt.cancel canceled;
                 enable_everything ();
                 match ~%client_callback with
                 | Some f -> Lwt.return (f res)
                 | None -> Lwt.return ()
               in ()
             with
             | Couldnt_unwrap -> Js.Unsafe.eval_string ("alert('Please fill all the required fields.')")
        ] in
        Html5.F.div [Html5.D.(Raw.form ~a:[a_onsubmit full_cb] [elt; myinput])]


  let make_parametrized: ('a, 'b) params_type -> ('c, 'd) params_type -> ('c -> 'a -> unit Lwt.t) -> 'c -> unit -> Widgets.div_content =
    fun params params_c callback ->
      let eliom_params, translator = to_eliom (TProd(params, params_c)) in
      let coservice: (('b * 'd), unit) cocaml = service_stub eliom_params (fun a -> let l, lc = translator a in
                                                                    callback lc l) in
      let rec build_form: type a b. (a, b) params_type -> form_content * ((unit -> b) Eliom_lib.client_value) = function
        | TAtom(name, default, TStringPassword) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Password; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TBool) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Checkbox; a_placeholder name; a_value (string_of_bool default)] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              bool_of_string @@ Js.to_string i##.value] : (unit -> bool) Eliom_lib.client_value)
        | TAtom(name, default, TString) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TInt) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              int_of_string @@ Js.to_string i##.value] : (unit -> int) Eliom_lib.client_value)
        | TAtomOpt(name, default_opt, TStringList(src_list, src_to_string)) ->
          let down_event =
            React.S.map (List.map src_to_string) src_list
            |> Eliom_react.S.Down.of_react in
          let select = [%client
            let open Eliom_content.Html5.D in
            ~%down_event
            |> React.S.map (fun l ->
              List.map (fun a ->
                option (pcdata a)
              ) l
              |> Raw.select
            )
            |> Html5.R.node
          ]
          in
          Obj.magic (Html5.C.node select), ([%client (fun () ->
               let i = Eliom_content.Html5.To_dom.of_select ~%select in
               Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TProd(a, b) -> 
          mk_prod a b
        | TAtom(_, _, TStringList(_)) -> failwith "stringlist must be optional"
        | TAtomOpt(_) -> failwith "optional not properly handled"
      and
        (* too much magic in there, anyone got any idea for that? *)
        mk_prod: type a b c d. ((a, c) params_type -> (b, d) params_type -> form_content * ((unit -> c * d) Eliom_lib.client_value)) = fun a b ->
          let a, fa = build_form a
          and b, fb = build_form b in
          Html5.F.div [a; b], Obj.magic (client_prod (Obj.magic fa) (Obj.magic fb))
      in
      let rec to_serialized: type a b. (a, b) params_type -> a -> b = function
        | TAtomOpt(name, _, TStringList(src_list, src_to_string)) -> src_to_string
        | TProd(a, b) -> (fun (data_a, data_b) ->
          to_serialized a data_a, to_serialized b data_b)
        | _ -> failwith "unable to serialize"
      in
      let to_serialized_c = to_serialized params_c in
      fun c () ->
        let d = to_serialized_c c in
        let elt, (get_values:(unit -> 'b) Eliom_lib.client_value) = build_form params in
        let full_cb = [%client fun e ->
             Dom.preventDefault e;
             try
               let _ = get_from_server ~%coservice (~%get_values (), ~%d) in ()
             with
             | Couldnt_unwrap -> Js.Unsafe.eval_string ("alert('Please fill all the required fields.')")
        ] in
        Html5.F.div [Html5.F.(Raw.form ~a:[a_onsubmit full_cb] [elt; Raw.input ~a:[a_input_type `Submit] ()])]

      

end
