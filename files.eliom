[%%shared
    open Lwt
    open Eliom_lib
    open Eliom_content
]

module Files(E:App_stub.ENV) : App_stub.FILES with type volume = E.Data.volume = struct

  open E.Data

  type volume = E.Data.volume

  let serv_volume = Hashtbl.create 10

  exception Volume_unknown_to_files of string


  let service =
    Eliom_service.App.service
      ~path: ["v"]
      ~get_params: Eliom_parameter.(suffix @@ string "volume") ()

  let service_for_volume n =
    try
        Hashtbl.find serv_volume (E.Data.volume_id n)
    with
    | Not_found -> raise (Volume_unknown_to_files (E.Data.volume_id n))

  let () =
    Eliom_registration.Any.register
      ~service
      (fun (volume_id:string) () ->
         try%lwt
           let open Widgets in
           
           let volume = E.Data.volume_from_id volume_id in

           let current_folder_client = Eliom_react.Up.create (Eliom_parameter.ocaml "a" Deriving_Json.Json_string.t) in

           let current_folder, current_folder_send = React.S.create "" in
           (* FIXME: that's a memory leak, we need to figure out how to keep it in the closure of the user session *)
           let () =
             Eliom_react.Up.to_react current_folder_client
             |> React.E.map current_folder_send
             |> Lwt_react.E.keep in
           
           let whole_file_list = E.Data.volume_list_files volume in
           let file_list =
             let open Shared in
             (* combining the two signals: either the file list changed, or the current folder changed *)
             React.S.l2 (fun a b ->
               List.filter (fun f ->
                 Shared.file_parent_path f = b) a
               ) whole_file_list current_folder
             |> Eliom_react.S.Down.of_react
           in
           let (volume_service:App_stub.path_service) = service_for_volume volume in
           let file_list = [%client
             React.S.map (fun l ->
               let all_files =
               l |> List.map (fun s ->
                 let open Shared in
                 if not s.is_folder then
                 let serv = (Eliom_service.preapply ~service:~%volume_service [s.name]) in
                 Html5.F.li [Html5.F.(a ~service:serv [pcdata s.name] ())]
                 else
                   let link = Widgets.C.link (fun () ->
                        ~%current_folder_client s.name
                   ) [Html5.F.(pcdata s.name)] in
                   Html5.F.li [link]
               )
               in
               let top = Widgets.C.link (fun () ->
                    ~%current_folder_client ""
               ) [Html5.F.(pcdata "..")] in
               Html5.F.li [top] :: all_files
             ) ~%file_list
             |> React.S.map (fun l ->
               Widgets.F.list_view l)
             |> Html5.R.node]
           in

           let ev = Eliom_react.S.Down.of_react Devices.all_devices in
           let (ul: Widgets.div_content
                     Eliom_pervasives.client_value) = [%client
             React.S.map  (fun l ->
               List.map (fun l -> Html5.F.(li [span [pcdata l]])) l) ~%ev
             |>
             React.S.map (fun l -> Widgets.F.list_view l)
             |> Html5.R.node]
           in
           E.F.main_box_sidebar [Widgets.F.two_panes (Html5.C.node file_list) (Html5.C.node ul)]
           >>= E.Config.App.send ~headers:Http_headers.(add (name "Cache-Control")  "max-age=6000" empty)
         with
         | E.Data.Volume_not_found _ ->
           E.F.main_box [Html5.F.p [Html5.F.pcdata "This volume does not seem to exist."]]
           >>= E.Config.App.send ~headers:Http_headers.(add (name "Cache-Control")  "max-age=6000" empty)
      )

  let register_service_for_volume n =
    Eliom_service.preapply ~service (E.Data.volume_id n)
    |> E.Mimes.register_public ("volume: " ^ E.Data.volume_id n)

  let on_new_volume vol =
    if public_volume vol then
      (* FIXME: Check if this path is available. This service needs to be saved in
         * order to be deleted afterwards. *)
      let service = Eliom_service.App.service
          ~path:["f";volume_id vol]
          ~get_params:Eliom_parameter.(suffix (all_suffix "path"))
          ()
      in
      Eliom_registration.File.register
        ~service
        (fun s () ->
           let s = Ocsigen_lib_base.Url_base.remove_dotdot s in
           return (volume_path vol ^ Ocsigen_lib.Url.string_of_url_path ~encode:false s));
      Hashtbl.add serv_volume (E.Data.volume_id vol) service
      ;
      register_service_for_volume vol

  let download_path vol filename =
    [filename]

  let _ =
    let _ = List.map on_new_volume (React.S.value E.Data.all_volumes) in
    React.E.map on_new_volume E.Data.new_volume_loaded
    |> Lwt_react.E.keep

  let () =
    E.Mimes.register_sidebar "volumes" (fun () ->

      let all_volumes_ev =
        React.S.map (fun all_volumes ->
          List.map E.Data.volume_id all_volumes) E.Data.all_volumes
        |> Eliom_react.S.Down.of_react in

      let volume_list =
        [%client
          let v:Html5_types.div_content_fun Eliom_content.Html5.F.elt = 
          ~%all_volumes_ev
          |> React.S.map (fun all_volumes ->
            all_volumes
            |> List.map (fun l ->
              let service = Eliom_service.preapply ~%service l in
              Html5.F.(li [a ~service [pcdata l] ()]))
            |> Widgets.F.list_view)
          |> Html5.R.node
          in
          v
        ] |> Html5.C.node
      in
      let h:Html5_types.div_content_fun Eliom_content.Html5.F.elt = Html5.F.(h1 [pcdata "volumes"]) in
      Lwt.return Html5.F.(div [h; volume_list]))

end

