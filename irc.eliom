[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

    open Irc_engine

]

open React
  
module IrcApp(Env:App_stub.ENVBASE) = struct

  module Irc_engine = Irc_engine(Env)

  let service =
    Eliom_service.App.service
      ~path: ["i"]
      ~get_params: Eliom_parameter.(suffix @@ string "channels") ()
  
  let main_service =
    Eliom_service.preapply ~service "all"

  let account_service =
    Eliom_service.App.service
      ~path: ["irc"]
      ~get_params: Eliom_parameter.(unit) ()

  let%lwt all_accounts = Env.Data.Objects.get_object_of_type irc_account_type

  let all_accounts_client =
    all_accounts
    |> React.S.map (List.map @@ Env.Data.Objects.get irc_account_type)
    |> Eliom_react.S.Down.of_react

  let create_account_form =
    Env.Form.(make (string "Server" "" ** int "Port" 3724 ** string "username" ""))
      (fun (server, (port, username)) ->
         let%lwt _ = Env.Data.Objects.save_object irc_account_type
             {server; port; username; realname=username; nick=username; }
         in return ()
      )

  let join_channel_form  =
    let get_server_name = (fun l ->
      let irc_account = Env.Data.Objects.get irc_account_type l in
      Format.sprintf "%s (%s)" irc_account.server irc_account.username
    )
    in
    Env.Form.(make (string "Channel" "" ** string_list "Server" all_accounts get_server_name))
      (fun (channel, account) ->
         let real_account = Env.Data.Objects.get irc_account_type account in
         let%lwt channel = Env.Data.Objects.save_object irc_channel_type
             {name=channel; server=real_account.server; }
         in
         Env.Data.Objects.link_to_parent account channel
      )

  let () =
    Env.Config.App.register
      ~service:account_service
      (fun () () ->
         let account_list =
           [%client
             let open Html5.F in
             ~%all_accounts_client
             |> React.S.map (
               List.map (fun account ->
                 li [pcdata account.server]
               )
             )
             |> React.S.map Widgets.F.list_view
             |> Html5.R.node
           ] |> Html5.C.node
         in
         Env.F.main_box_sidebar [account_list; create_account_form (); join_channel_form ()]
      )
  
  
  let () =
    Env.Config.App.register
      ~service
      (fun (channel) () ->
         if channel = "all" then
           let%lwt irc_messages = Env.Data.Objects.get_object_of_type irc_message_type
           in
           let%lwt all_irc_channels = Env.Data.Objects.get_object_of_type irc_channel_type in
           let%lwt all_irc_channels  =
             all_irc_channels
             |> Lwt_react.S.map_s @@ Lwt_list.map_s (fun channel_object ->
               let%lwt channel_messages = Env.Data.Objects.object_get_all_children channel_object irc_message_type in
               Lwt_react.S.map (fun msgs ->
                 channel_object, msgs) channel_messages
               |> return
             )
             >>= (fun s -> s
                           |> React.S.map @@ React.S.merge (fun l s -> s :: l) []
                           |> React.S.switch
                           |> React.S.map @@ List.map (fun (l, c) -> Env.Data.Objects.get irc_channel_type l, (List.map (Env.Data.Objects.get irc_message_type)) c)
                           |> React.S.map @@ List.map (fun (l, c) -> l, List.sort (fun i j -> compare i.timestamp j.timestamp) c)
                           |> Eliom_react.S.Down.of_react
                           |> return)
           in
           let all_messages =
             [%client
               ~%all_irc_channels
               |> React.S.map (List.map (fun (l, c) ->
                 let all_messages = 
                   c
                   |> List.map (fun l ->
                     Html5.F.(li [pcdata l.content])
                   )
                   |> Html5.F.ul
                 in
                 Html5.F.(li [h1 [pcdata l.name]; all_messages])
               ))
               |> React.S.map (fun l ->
                 Html5.F.ul l)
               |> Html5.R.node
             ] |> Html5.C.node
           in
           Env.F.main_box_sidebar [all_messages]
         else
           let h = Html5.F.h1 [pcdata channel] in
           let%lwt irc_messages = Env.Data.Objects.get_object_of_type irc_message_type in
           let irc_messages = irc_messages
                              |> React.S.map (List.map (Env.Data.Objects.get irc_message_type))
                              |> React.S.map (List.filter (fun s -> s.target.channel = channel))
                              |> Eliom_react.S.Down.of_react
           in
           let messages =
             [%client
               ~%irc_messages
               |> React.S.map (List.map (fun l ->
                 Html5.F.(li [pcdata l.content])
               ))
               |> React.S.map Html5.F.ul
               |> Html5.R.node
             ] |> Html5.C.node
           in
           Env.F.main_box_sidebar [h; messages]
      )
  
  let () =
    Env.Mimes.register_sidebar "irc" (fun () ->

      let%lwt all_irc_channels = Env.Data.Objects.get_object_of_type irc_channel_type in
      let all_irc_ev =
        all_irc_channels
        |> React.S.map (List.map @@Env.Data.Objects.get irc_channel_type)
        |> Eliom_react.S.Down.of_react in

      let channel_list =
        [%client
          ~%all_irc_ev
          |> React.S.map (fun all_chans ->
            all_chans
            |> List.map (fun l ->
              let service = Eliom_service.preapply ~%service l.name in
              Html5.F.(li [a ~service [pcdata l.name] ()]))
            |> List.rev
            |> (fun l ->
              Html5.F.(li [a ~service:~%account_service [pcdata "Settings"] () ]) :: l)
            |> List.rev
            |> Widgets.F.list_view)
          |> Html5.R.node
        ] |> Html5.C.node
      in
      let h:Html5_types.div_content_fun Eliom_content.Html5.F.elt = Html5.F.(h1 [pcdata "irc"]) in
      Lwt.return Html5.F.(div [h; channel_list]))

end
