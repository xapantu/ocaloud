[%%shared
    open Eliom_content
    open Eliom_lib
    type 'a cocaml = (unit, 'a, Eliom_service.service_method,
                      Eliom_service.attached,
                      Eliom_service.service_kind,
                      [ `WithoutSuffix ], unit, string,
                      Eliom_service.registrable,
                      unit Eliom_service.ocaml_service) Eliom_service.service
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
    | TAtom(n, _, TInt) -> Obj.magic @@ (Eliom_parameter.int n, fun i -> i)
    | TProd(a, b) ->
      let p1, t1 = to_eliom a in
      let p2, t2 = to_eliom b in
      Obj.magic @@ (Eliom_parameter.(p1 ** p2), fun (a, b) -> t1 a, t2 b)
    | TAtomOpt(n, _, TStringList(src_list, to_string)) ->
      Obj.magic @@ (Eliom_parameter.string n, (fun i ->
        List.find (fun a -> to_string a = i) (React.S.value src_list)))

  let make: ('a, 'b) params_type -> ('a -> unit Lwt.t) -> unit -> Widgets.div_content =
    fun params callback ->
      let eliom_params, (translator: 'b -> 'a) = to_eliom params in
      let coservice: 'b cocaml = service_stub eliom_params (fun l -> callback (translator l)) in
      let rec build_form: type a b. (a, b) params_type -> form_content * ((unit -> b) Eliom_lib.client_value) = function
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
      and
        (* too much magic in there, anyone got any idea for that? *)
        mk_prod: type a b c d. ((a, c) params_type -> (b, d) params_type -> form_content * ((unit -> c * d) Eliom_lib.client_value)) = fun a b ->
          let a, fa = build_form a
          and b, fb = build_form b in
          Html5.F.div [a; b], Obj.magic (client_prod (Obj.magic fa) (Obj.magic fb))
      in
      fun () ->
        let elt, (get_values:(unit -> 'b) Eliom_lib.client_value) = build_form params in
        let full_cb = [%client fun e ->
             Dom.preventDefault e;

             try
               let _ = get_from_server (~%coservice:string cocaml) (~%get_values ()) in ()
             with
             | Couldnt_unwrap -> Js.Unsafe.eval_string ("alert('Please fill all the required fields.')")
        ] in
        Html5.F.div [Html5.F.(Raw.form ~a:[a_onsubmit full_cb] [elt; Raw.input ~a:[a_input_type `Submit] ()])]
      

end
