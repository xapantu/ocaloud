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
  let s =
    try
      let i = String.index s '!' in
      String.sub s 0 i
    with
    | Not_found -> s
  in
  s, (Format.sprintf "color:#%6x" (Hashtbl.hash s mod 0xaacccc))


let visiblitychange (f:Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) =
  let ev = Dom.Event.make "visibilitychange" in
  let vi = Lwt_js_events.make_event ev in
  Lwt_js_events.seq_loop vi Dom_html.document f
]

open React
  
module IrcApp(Env:App_stub.ENVBASE) = struct

  module Irc_engine = Irc_engine(Env)
  open Irc_engine

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
                   Irc_engine.send_to_channel channel (Irc_engine.new_message content "me")  ) (Some (fun () -> [%client fun _ -> App_stub.Clear])) )

  let join_channel_form  =
    let get_server_name = (fun l ->
      let irc_account = Env.Data.Objects.get irc_account_type l in
      Format.sprintf "%s (%s)" irc_account.server irc_account.username
    )
    in
    Env.Form.(make (string "Channel" "" ** string "Password" "" ** string_list "Server" all_accounts get_server_name))
      (fun (channel, (password, account)) ->
         let real_account = Env.Data.Objects.get irc_account_type account in
         let password = if password = "" then None else Some password in
         let%lwt channel = Env.Data.Objects.save_object irc_channel_type
             {name=channel; opened = true; server=real_account.server; password; }
         in
         Env.Data.Objects.link_to_parent account irc_account_type channel irc_channel_type
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

  let mark_as_read_service =
    Eliom_service.Ocaml.post_coservice' ~post_params:(Eliom_parameter.string "channel") ()

  let mark_as_closed =
    Eliom_service.Ocaml.post_coservice' ~post_params:(Eliom_parameter.string "channel") ()

  let () =
    Eliom_registration.Ocaml.register
      ~service:mark_as_read_service
      (fun () irc_channel_id ->
         let%lwt objs = Env.Data.Objects.get_object_of_type irc_channel_type in
         let c =
           objs
           |> React.S.value
           |> List.find (fun i ->
             Env.Data.Objects.get_id_as_string i = irc_channel_id)
         in
         let%lwt irc_messages = Env.Data.Objects.object_get_all_children c irc_message_type in
         let l = React.S.value irc_messages in
         let a, b =
           List.map (fun i ->
             let di = Env.Data.Objects.get irc_message_type i in
             di, i) l
           |> List.filter (fun (d, _) -> not d.read)
           |> List.map (fun (d, i) -> { d with read = true }, i)
           |> List.split in
         Env.Data.Objects.update_objects irc_message_type b a
      )

  let () = 
    Eliom_registration.Ocaml.register
      ~service:mark_as_closed
      (fun () irc_channel_id ->
         let%lwt objs = Env.Data.Objects.get_object_of_type irc_channel_type in
         let c =
           objs
           |> React.S.value
           |> List.find (fun i ->
             Env.Data.Objects.get_id_as_string i = irc_channel_id)
         in
         let d = Env.Data.Objects.get irc_channel_type c in
         let d = { d with opened = false } in
         Env.Data.Objects.update_object irc_channel_type c d
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
             let channel_id = Env.Data.Objects.get_id_as_string channel in
             let%lwt irc_messages = Env.Data.Objects.object_get_all_children channel irc_message_type in
             let irc_messages = irc_messages
                                |> React.S.map (List.map (Env.Data.Objects.get irc_message_type))
                                |> React.S.map (List.sort (fun i j -> compare j.timestamp i.timestamp))
                                |> React.S.map @@ list_truncate 500
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
                   let auth, color = extract_author l.author in
                   Html5.F.(div [span [pcdata (Js.to_string t)]; span ~a:[a_style color] [pcdata " "; pcdata auth]; span content])
                 ))
                 |> React.S.map Html5.F.div
                 |> Html5.R.node
                 |> fun a -> Html5.D.div ~a:[Html5.D.a_class ["irc-view"]] [a]
               in
               ~%irc_messages 
               |> React.S.map (fun _ ->
                 (* yes, that's a hack *)
                 let div = Eliom_content.Html5.To_dom.of_div message_div in
                 let%lwt () = Lwt_js.sleep 0.1 in
                 (div##.scrollTop := (div##.scrollHeight));
                 if div##.offsetWidth > 0 && not (Js.Unsafe.get (Dom_html.document) "hidden") then
                   begin
                     let%lwt () = Eliom_client.call_ocaml_service ~service:~%mark_as_read_service () ~%channel_id in
                     Lwt.return_unit
                   end
                 else Lwt.return_unit
               )
               |> Lwt_react.S.keep;
                 Lwt.async (fun () ->
                   Lwt_js_events.onresizes (fun _ _ ->
                     let div = Eliom_content.Html5.To_dom.of_div message_div in
                     return (div##.scrollTop := (div##.scrollHeight));
                   )
               );
               Lwt.async (fun () ->
                 visiblitychange (fun _ _ ->
                     let%lwt () = Eliom_client.call_ocaml_service ~service:~%mark_as_read_service () ~%channel_id in
                     Lwt.return_unit
                 );
               );
               message_div
               ] |> Html5.C.node
             in
             Env.F.flex_box_sidebar [h; Html5.D.(div ~a:[a_class ["irc-user-message"]] [userlist; messages]); Html5.D.(div ~a:[a_class ["irc-entry"]] [send_message_form channel ()])]
         end
      )
  
  let () =
    Env.Mimes.register_sidebar "irc" (fun () ->

      if User.is_logged () then
        let%lwt all_irc_channels =
          Env.Data.Objects.get_object_of_type irc_channel_type
          >|= React.S.map (List.filter (fun c -> (Env.Data.Objects.get irc_channel_type c).opened)) in
        let%lwt all_irc_ev =
          Lwt_react.S.bind_s all_irc_channels (fun l ->
            let%lwt chans =
              Lwt_list.map_s begin fun o ->
                let real_obj = Env.Data.Objects.get irc_channel_type o in
                let%lwt children = Env.Data.Objects.object_get_all_children o irc_message_type in
                let children = children
                               |> React.S.map (List.filter (fun i ->
                                 not (Env.Data.Objects.get irc_message_type i).read))
                               |> React.S.map List.length
                               |> React.S.map (fun l -> real_obj, l)
                in
                Lwt.return children
              end l
            in
            Lwt.return @@ React.S.merge (fun a b -> b::a) [] chans )
        in
        let all_irc_ev = Offline.down_of_react all_irc_ev in

        let channel_list =
          [%client
          Offline.if_online "sidebar_irc"  (fun () -> ~%all_irc_ev) [{server = "offline_server"; name ="offline_chan"; opened = true; password = None; }, 42]
          |> React.S.map (fun all_chans ->
            all_chans
            |> List.map (fun ((l:irc_channel), (unread_count:int)) ->
              let service = Eliom_service.preapply ~%service (l.server, l.name) in
              Html5.F.(li [a ~service [pcdata l.name; pcdata @@ Format.sprintf " (%d)" unread_count] ()]))
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
