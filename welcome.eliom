[%%shared 
open Lwt
open Eliom_lib
open Eliom_content
]
open Html5
open F

module Welcome(E:App_stub.ENV) = struct

  module Dumb_password = Dumb_password.Dumb_password(E)

  let main_service =
    Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

  let () =
    E.Config.App.register
      ~service:main_service
      (fun () () ->
         let all_public = E.Mimes.get_all_public_services ()
                          |> List.map (fun (name, service) ->
                            a service [pcdata name] ()
                          ) in
         let logged =
           User.login_signal ()
           |> Eliom_react.S.Down.of_react
         in
         let login_widget = Dumb_password.main_widget () in
         let logged_mention =
           [%client
             ~%logged
             |> React.S.map begin function
               | Some l ->
                 Html5.F.(span [pcdata l])
               | None ->
                 Html5.F.(div [~%login_widget; span [pcdata "unlogged"]]) end
             |> Html5.R.node
           ] |> Html5.C.node in
         E.F.main_box [logged_mention; div all_public]
      )
end

[%%client
    open Dumb_password
]
