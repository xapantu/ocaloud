[%%shared
    open React
    open Lwt

    type irc_message = {
      content:string; [@key 1]
      timestamp:float; [@key 2]
      author: string; [@key 3]
    } [@@deriving protobuf]

    type irc_channel = {
      name:string; [@key 1]
      server:string; [@key 2]
    } [@@deriving protobuf]

    type irc_account = {
      server: string; [@key 1]
      port: int; [@key 2]
      username: string; [@key 3]
      realname: string; [@key 4]
      nick: string; [@key 5]
    } [@@deriving protobuf]
]

let irc_message_type = "irc-message", irc_message_from_protobuf, irc_message_to_protobuf
let irc_channel_type = "irc-channel", irc_channel_from_protobuf, irc_channel_to_protobuf
let irc_account_type = "irc-account", irc_account_from_protobuf, irc_account_to_protobuf

module Irc = Irc_client_lwt_ssl

open Lwt_unix

let unwrap = function
  | Some x -> x
  | None -> failwith "No value"

module Irc_engine(Env:App_stub.ENVBASE) = struct

  let%lwt all_channels = Env.Data.Objects.get_object_of_type irc_channel_type

  let get_channel account name =
    try
      List.find (fun p -> (Env.Data.Objects.get irc_channel_type p).name = name) (React.S.value all_channels)
      |> return
    with
    | Not_found -> Env.Data.Objects.save_object irc_channel_type {name; server = (Env.Data.Objects.get irc_account_type account).server;}
  
  let new_message content author = 
    {content; timestamp = Unix.gettimeofday (); author; }

  let ping_server account connection =
    let open Irc_message in
    let open Env.Data.Objects in
    let save_message = save_object irc_message_type in
    let server = (get irc_account_type account).server in
    Ocsigen_messages.errlog "connecting.";
    let%lwt a = 
    Irc.listen ~connection ~callback:(
      fun connection result ->
        match result with
        | `Ok ({ command = PRIVMSG (channel, msg) ; prefix = Some author; } as e)->
          let msg = String.trim msg in
          let%lwt target_channel = get_channel account channel in
          let%lwt msg_obj = save_message (new_message msg author) in
          let%lwt () = link_to_parent target_channel msg_obj in
          let%lwt msg_obj = save_message (new_message (to_string e) author ) in
          let%lwt target_channel = get_channel account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
        | `Ok e ->
          let msg = new_message (to_string e) "server" in
          let%lwt msg_obj = save_message msg in
          let%lwt target_channel = get_channel account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
        | `Error e ->
          let msg = new_message e "error" in
          let%lwt msg_obj = save_message msg in
          let%lwt target_channel = get_channel account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
    )
    in
    Ocsigen_messages.errlog "disconnected."; Lwt.return a

  
  let all_connection_handles =
    let%lwt all_accounts = Env.Data.Objects.get_object_of_type irc_account_type in
    let open ReactiveData in
    let handle = RList.from_signal all_accounts in
    RList.map (fun l ->
      let irc_account = Env.Data.Objects.get irc_account_type l in
      let server, port, username, realname, nick =
        irc_account.server, irc_account.port, irc_account.username, irc_account.realname, irc_account.nick in
      Ocsigen_messages.accesslog (Format.sprintf  "connecting to %s with username %s" server username);
      Irc.connect_by_name ~server ~port ~username ~mode:0 ~realname ~nick ()
      >>= fun connection -> return (unwrap connection, l)
    ) handle |> return

  let all_connections =
    let open ReactiveData in
    let%lwt con = all_connection_handles in
    return (RList.signal con)

  let _ =
    let open ReactiveData in
    let%lwt con = all_connection_handles in
    RList.map (fun a ->
      a >>= fun (connection, account) ->
          let server = (Env.Data.Objects.get irc_account_type account).server in
          let%lwt chans =
            let%lwt all_chans = Env.Data.Objects.object_get_all_children account irc_channel_type in
            React.S.map (fun l ->
              List.map (Env.Data.Objects.get irc_channel_type) l) all_chans
            |> ReactiveData.RList.from_signal
            |> RList.map (fun channel ->
              Ocsigen_messages.errlog (Format.sprintf "joining %s on server %s" channel.name server);
              let channel = channel.name in
              Irc.send_join ~connection ~channel)
            |> return
          in
          ping_server account connection >>= fun a -> Lwt.return (a, chans)
    ) con |> RList.signal |> Lwt_react.S.keep |> return

  
  let send_to_channel channel msg =
    let open Env.Data.Objects in
    let%lwt msg_obj = save_object irc_message_type msg in
    let%lwt () = link_to_parent channel msg_obj in
    let%lwt account = get_parent irc_account_type channel in
    let channel = get irc_channel_type channel in
    let%lwt all_connections = all_connections in
    Lwt_list.map_s (fun a -> a) (React.S.value all_connections)
    >>= Lwt_list.find_s (fun (_, l) ->
      return (l = account)) 
    >>= fun (connection, account) ->
        Irc.send_privmsg ~connection ~target:channel.name ~message:msg.content

  

end
