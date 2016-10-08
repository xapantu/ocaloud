[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

    open Irc_engine

    let rec list_truncate n = function
      | [] -> []
      | t::q -> if n <= 0 then []
        else t :: (list_truncate (n-1) q)

]

[%%client
let extract_author s =
  try
    let i = String.index s '!' in
    String.sub s 0 i
  with
  | Not_found -> s
]

open React
  
module IrcApp(Env:App_stub.ENVBASE) = struct

  module Irc_engine = Irc_engine(Env)

  let service =
    Eliom_service.App.service
      ~path: ["i"]
      ~get_params: Eliom_parameter.(suffix @@ string "account" ** string "channels") ()
  
  let main_service =
    Eliom_service.preapply ~service ("all", "all")

  let account_service =
    Eliom_service.App.service
      ~path: ["irc"]
      ~get_params: Eliom_parameter.(unit) ()

  let%lwt all_accounts = Env.Data.Objects.get_object_of_type irc_account_type
  let%lwt all_channels = Env.Data.Objects.get_object_of_type irc_channel_type

  let all_accounts_client =
    all_accounts
    |> React.S.map (fun l ->
      let signals =
      List.map (fun account ->
             Env.Data.Objects.object_get_all_children account irc_connected_type
             >|= React.S.map (fun states ->
               (try
                 List.map (Env.Data.Objects.get irc_connected_type) states
                 |> List.sort (fun a b -> compare (b:irc_connected).time a.time)
                 |> List.hd
                 |> fun c -> c.state
               with
               | Not_found | Failure "hd" -> Disconnected),
               Env.Data.Objects.get irc_account_type account
             )
           |> Lwt_main.run
        ) l
      in
      React.S.merge (fun l a -> a::l) [] signals
    )
  |> React.S.switch
  |> Eliom_react.S.Down.of_react

  let create_account_form =
    Env.Form.(make (string "Server" "" ** int "Port" 3724 ** string "username" ""))
      (fun (server, (port, username)) ->
         let%lwt _ = Env.Data.Objects.save_object irc_account_type
             {server; port; username; realname=username; nick=username; }
         in return ()
      ) None

  let send_message_form =
    Env.Form.(make_parametrized (string "content" "") (string_list "channel" all_channels Env.Data.Objects.get_id_as_string)
                (fun channel content ->
                   Irc_engine.send_to_channel channel (Irc_engine.new_message content "me")  ) (Some ([%client fun _ -> App_stub.Clear])) )

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
      ) None

  let () =
    Env.Config.App.register
      ~service:account_service
      (fun () () ->
         Env.Permissions.ensure_role "logged"
         begin fun () ->
             let account_list =
               [%client
                 let open Html5.F in
                 ~%all_accounts_client
                 |> React.S.map (
                   List.map (fun (state, account) ->
                     let state = match state with
                       | Connected -> pcdata ": connected"
                       | Disconnected -> pcdata ": disconnected"
                       | Connecting -> pcdata ": connecting"
                     in
                     li [pcdata account.server; state]
                   )
                 )
                 |> React.S.map Widgets.F.list_view
                 |> Html5.R.node
               ] |> Html5.C.node
             in
             Env.F.main_box_sidebar [account_list; create_account_form (); join_channel_form ()]
         end
      )
  
  
  let () =
    Env.Config.App.register
      ~service
      (fun (account, channelname) () ->
         Env.Permissions.ensure_role "logged" begin
           fun () ->
             let channel = List.find (fun l -> (Env.Data.Objects.get irc_channel_type l).name = channelname) (React.S.value all_channels) in
             let%lwt account = Env.Data.Objects.get_parent irc_account_type channel in
             let%lwt account_state =
               Env.Data.Objects.object_get_all_children account irc_connected_type
               >|= React.S.map (fun states ->
                 try
                   List.map (Env.Data.Objects.get irc_connected_type) states
                   |> List.sort (fun a b -> compare (b:irc_connected).time a.time)
                   |> List.hd
                   |> fun c -> c.state
                 with
                 | Not_found | Failure "hd" -> Disconnected
               )
               >|= Eliom_react.S.Down.of_react
             in
             let%lwt user_list =
               Env.Data.Objects.object_get_all_children channel irc_user_list_type
               >|= React.S.map (fun users ->
                 try
                   List.map (Env.Data.Objects.get irc_user_list_type) users
                   |> List.sort (fun a b -> compare (b:irc_user_list).time a.time)
                   |> List.hd
                   |> fun c -> c.users
                 with
                 | Not_found | Failure "hd" -> []
               )
               >|= Eliom_react.S.Down.of_react
             in
             let h = 
               [%client
               React.S.map (function
                 | Disconnected ->
                   Html5.F.h1 [pcdata ~%channelname; pcdata " (disconnected)"]
                 | Connecting ->
                   Html5.F.h1 [pcdata ~%channelname; pcdata " (connecting)"]
                 | Connected ->
                   Html5.F.h1 [pcdata ~%channelname; pcdata " (connected)"] 
               ) ~%account_state
               |> Html5.R.node
               ] |> Html5.C.node in
             let userlist =
               [%client
               React.S.map (fun l ->
                 Html5.F.ul (List.map (fun u ->
                   li [pcdata u]) l)
               ) ~%user_list
               |> Html5.R.node
               ] |> Html5.C.node in
             let%lwt irc_messages = Env.Data.Objects.object_get_all_children channel irc_message_type in
             let irc_messages = irc_messages
                                |> React.S.map (List.map (Env.Data.Objects.get irc_message_type))
                                |> React.S.map (List.sort (fun i j -> compare j.timestamp i.timestamp))
                                |> React.S.map @@ list_truncate 100
                                |> React.S.map (fun l ->
                                  match l with
                                  | t::q ->
                                    let i, l = List.fold_left (fun (old_i, l) i ->
                                      if abs_float (old_i.timestamp -. i.timestamp) < 60. && old_i.author = i.author then
                                        let i = { i with content = old_i.content ^ "\n" ^ i.content } in
                                        i, l
                                      else
                                        i, (old_i::l)
                                    ) (t, []) q
                                    in
                                    i::l
                                  | [] -> []
                                )
                                |> Eliom_react.S.Down.of_react
             in
             let messages =
               [%client
               let message_div =
                 ~%irc_messages
                 |> React.S.map (List.map (fun l ->
                   let t = Js.Unsafe.eval_string (Format.sprintf "(new Date(%f)).toLocaleTimeString([], {hour: '2-digit', minute: '2-digit'})" (l.timestamp*.1000.) ) in
                   let content = Irc_engine.split_on_char '\n' l.content |> List.fold_left (fun l a -> Html5.F.pcdata a:: Html5.F.br () :: l) [] in
                   Html5.F.(div [span [pcdata (Js.to_string t)]; span [pcdata " "; pcdata (extract_author l.author)]; span content])
                 ))
                 |> React.S.map Html5.F.div
                 |> Html5.R.node
                 |> fun a -> Html5.D.div ~a:[Html5.D.a_class ["irc-view"]] [a]
               in
               ~%irc_messages 
               |> React.S.map (fun _ ->
                 (* yes, that's a hack *)
                 let%lwt () = Lwt_js.sleep 0.1 in
                 let div = Eliom_content.Html5.To_dom.of_div message_div in
                 return (div##.scrollTop := (div##.scrollHeight));
               )
               |> Lwt_react.S.keep;
               message_div
               ] |> Html5.C.node
             in
             Env.F.flex_box_sidebar [h; Html5.D.(div ~a:[a_class ["irc-user-message"]] [userlist; messages]); Html5.D.(div ~a:[a_class ["irc-entry"]] [send_message_form channel ()])]
         end
      )
  
  let () =
    Env.Mimes.register_sidebar "irc" (fun () ->

      if User.is_logged () then
        let%lwt all_irc_channels = Env.Data.Objects.get_object_of_type irc_channel_type in
        let all_irc_ev =
          all_irc_channels
          |> React.S.map (List.map @@ Env.Data.Objects.get irc_channel_type)
          |> Offline.down_of_react in

        let channel_list =
          [%client
          Offline.if_online (fun () -> ~%all_irc_ev) [{server = "offline_server"; name ="offline_chan"; }]
          |> React.S.map (fun all_chans ->
            all_chans
            |> List.map (fun (l:irc_channel) ->
              let service = Eliom_service.preapply ~%service (l.server, l.name) in
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
        Lwt.return Html5.F.(div [h; channel_list])
      else
        Lwt.return Html5.F.(div [])
    );
    Env.Mimes.register_public "irc" account_service; ()

end
