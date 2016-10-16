[%%client
let _ =
  Js.Unsafe.eval_string "console.log('registering key press')";
  Lwt_js_events.async (fun () ->
    Lwt_js_events.keypresses Dom_html.document (fun e _ ->
      if Js.to_bool (e##.altKey) then
        begin
          Dom.preventDefault e;
          let key = Js.Unsafe.get e "key" in
          let b = Js.to_bool (e##.altKey) in
          Lwt.return (Js.Unsafe.eval_string ("console.log('" ^ (string_of_int key) ^ "key_press')"));
          ()
        end;
      Lwt.return_unit))
]

module Mimes(Config:App_stub.CONFIG) = struct
  let services = Hashtbl.create 20

  let register_public (name:string) (service:App_stub.unit_service) = Hashtbl.add services "public" (name, service)

  let get_all_public_services () =
    Hashtbl.find_all services "public"

  let sidebar: (unit -> Html5_types.div Eliom_content.Html5.elt Lwt.t) list ref = ref []

  let register_sidebar name f =
    sidebar := f :: !sidebar

  let build_sidebar () =
    let _ = [%client Js.Unsafe.eval_string "console.log('test')" ] in
    Lwt_list.map_s ((|>) ()) !sidebar

end
