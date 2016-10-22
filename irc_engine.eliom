[%%shared
    open React
    open Lwt

    type irc_message = {
      content:string [@key 1];
      timestamp:float [@key 2];
      author: string [@key 3];
      read: bool [@key 4];
    } [@@deriving protobuf]

    type irc_channel = {
      name:string [@key 1];
      server:string [@key 2];
      opened: bool [@key 3];
    } [@@deriving protobuf]

    type irc_account = {
      server: string [@key 1];
      port: int [@key 2];
      username: string [@key 3];
      realname: string [@key 4];
      nick: string [@key 5];
    } [@@deriving protobuf]

    type connection_state =
      | Connected [@key 1]
      | Connecting [@key 2]
      | Disconnected [@key 3]
    [@@deriving protobuf]

    type irc_connected = {
      state: connection_state [@key 1];
      time: float [@key 2];
    } [@@deriving protobuf]

    type irc_user_list = {
      users: string list [@key 1];
      time: float [@key 2];
    } [@@deriving protobuf]

]

module Irc = Irc_client_lwt_ssl

open Lwt_unix

let unwrap = function
  | Some x -> x
  | None -> failwith "No value"

[%%shared
let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r
]

let extract_author s =
  try
    let i = String.index s '!' in
    String.sub s 0 i
  with
  | Not_found -> s

module Irc_engine(Env:App_stub.ENVBASE) = struct

  open ReactiveData
  
  let irc_message_type = Env.Data.Objects.create_object_type "irc-message" irc_message_from_protobuf irc_message_to_protobuf
  let irc_channel_type = Env.Data.Objects.create_object_type "irc-channel" irc_channel_from_protobuf irc_channel_to_protobuf
  let irc_account_type = Env.Data.Objects.create_object_type "irc-account" irc_account_from_protobuf irc_account_to_protobuf
  let irc_connected_type = Env.Data.Objects.create_object_type "irc-connected" irc_connected_from_protobuf irc_connected_to_protobuf
  let irc_user_list_type = Env.Data.Objects.create_object_type "irc-user-list" irc_user_list_from_protobuf irc_user_list_to_protobuf

  let%lwt all_channels = Env.Data.Objects.get_object_of_type irc_channel_type

  let get_channel account name =
    try
      List.find (fun p -> (Env.Data.Objects.get irc_channel_type p).name = name) (React.S.value all_channels)
      |> return
    with
    | Not_found ->
      let%lwt channel = Env.Data.Objects.save_object irc_channel_type {name; opened = true; server = (Env.Data.Objects.get irc_account_type account).server;} in
      let%lwt () = Env.Data.Objects.link_to_parent account irc_account_type channel irc_channel_type in
      Lwt.return channel
  
  let new_message content author = 
    {content; timestamp = Unix.gettimeofday (); author; read = false; }

  let ping_server account connection =
    let open Irc_message in
    let open Env.Data.Objects in
    let save_message = save_object irc_message_type in
    let server = (get irc_account_type account).server in
    let nick = (get irc_account_type account).nick in
    let users = Hashtbl.create 100 in
    let save_users channel =
      let%lwt user_list = Env.Data.Objects.save_object irc_user_list_type { users = Hashtbl.find users channel; time = Unix.gettimeofday () } in
      let%lwt channel = get_channel account channel in
      Env.Data.Objects.link_to_parent channel irc_channel_type user_list irc_user_list_type
    in
    let get_users channel =
      try
        Hashtbl.find users channel
      with
      | Not_found -> Hashtbl.add users channel []; []
    in
    let add_user channel user =
      let l = get_users channel in
      Hashtbl.replace users channel (user::l);
      save_users channel
    in
    let add_users channel users' =
      let l = get_users channel in
      Hashtbl.replace users channel (users' @ l);
      save_users channel
    in
    let rm_user channel user =
      let l = get_users channel in
      Hashtbl.replace users channel (List.filter ((<>) user) l);
      save_users channel
    in
    Ocsigen_messages.errlog "connecting.";
    let%lwt a = 
      Irc.listen ~connection ~callback:(
        fun connection result ->
          match result with
          | `Ok ({ command = PRIVMSG (channel, msg) ; prefix = Some author; })->
            let msg = String.trim msg in
            let%lwt target_channel =
              if channel = nick then
                get_channel account (extract_author author)
              else 
                get_channel account channel in
            let%lwt msg_obj = save_message (new_message msg author) in
            let%lwt () = link_to_parent target_channel irc_channel_type  msg_obj irc_message_type in
            return ()
          | `Ok ({ command = Other ("353", params) ; _ }) when List.length params > 3 ->
            let channel = List.nth params 2 in
            let users = List.nth params 3 |> split_on_char ' ' in
            add_users channel users
          | `Ok ({ command = PART (channels, msg) ; prefix = Some author; })->
            let author = extract_author author in
            Lwt_list.iter_s (fun i ->
              let%lwt target_channel = get_channel account i in
              let%lwt msg_obj = save_message (new_message (Format.sprintf "%s left. (%s)" author msg) "") in
              let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
              rm_user i author) channels
          | `Ok ({ command = QUIT (msg) ; prefix = Some author; })->
            let author = extract_author author in
            Hashtbl.fold (fun i _ l ->
              let%lwt () = l in
              let%lwt target_channel = get_channel account i in
              let%lwt msg_obj = save_message (new_message (Format.sprintf "%s quit. (%s)" author msg) "") in
              let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
              rm_user i author) users Lwt.return_unit
          | `Ok ({ command = NICK (new_name) ; prefix = Some author; })->
            let author = extract_author author in
            Hashtbl.fold (fun i _ l ->
              let%lwt () = l in
              let%lwt target_channel = get_channel account i in
              let%lwt msg_obj = save_message (new_message (Format.sprintf "%s is now known as %s." author new_name) "") in
              let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
              let%lwt () = rm_user i author in
              add_user i new_name) users Lwt.return_unit
          | `Ok ({ command = JOIN (channels, _) ; prefix = Some author; })->
            let author = extract_author author in
            if author <> nick then
              Lwt_list.iter_s (fun i ->
                let%lwt target_channel = get_channel account i in
                let%lwt msg_obj = save_message (new_message (Format.sprintf "%s joined." author) "") in
                let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
                add_user i author) channels
            else
              Lwt_list.iter_s (fun i ->
                let%lwt target_channel = get_channel account i in
                let%lwt msg_obj = save_message (new_message (Format.sprintf "%s joined." author) "") in
                link_to_parent target_channel irc_channel_type  msg_obj irc_message_type) channels
          | `Ok e ->
            Ocsigen_messages.errlog (to_string e);
            let msg = new_message (to_string e) "" in
            let%lwt msg_obj = save_message msg in
            let%lwt target_channel = get_channel account server in
            let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
            return ()
          | `Error e ->
            let msg = new_message e "error" in
            let%lwt msg_obj = save_message msg in
            let%lwt target_channel = get_channel account server in
            let%lwt () = link_to_parent target_channel irc_channel_type msg_obj irc_message_type in
            return ()
      )
    in
    Ocsigen_messages.errlog "disconnected."; Lwt.return a

  
  let all_connection_handles =
    let%lwt all_accounts = Env.Data.Objects.get_object_of_type irc_account_type in
    let handle = RList.from_signal all_accounts in
    RList.map (fun l ->
      let%lwt () = Env.Data.Objects.save_object irc_connected_type { state = Connecting; time = Unix.gettimeofday (); }
        >>= fun o -> Env.Data.Objects.link_to_parent l irc_account_type o irc_connected_type 
      in
      let irc_account = Env.Data.Objects.get irc_account_type l in
      let server, port, username, realname, nick =
        irc_account.server, irc_account.port, irc_account.username, irc_account.realname, irc_account.nick in
      Ocsigen_messages.accesslog (Format.sprintf  "connecting to %s with username %s" server username);
      Irc.connect_by_name ~server ~port ~username ~mode:0 ~realname ~nick ()
      >>= fun connection ->
          match connection with
          | Some _ ->
            return (connection, l)
          | None ->
            Ocsigen_messages.accesslog ("Couldn't connect to irc server");
            Lwt_unix.sleep 1.0 
            >>= fun () -> Env.Data.Objects.save_object irc_connected_type { state = Disconnected; time = Unix.gettimeofday (); }
            >>= fun o -> Env.Data.Objects.link_to_parent l irc_account_type o irc_connected_type
            >>=  fun () -> return (connection, l)
    ) handle |> return

  let all_connections =
    let%lwt con = all_connection_handles in
    return (RList.signal con)

  let _ =
    let%lwt con = all_connection_handles in
    RList.map (fun a ->
      a >>= fun (connection, account) ->
      match connection with
      | Some connection ->
        let%lwt () = Env.Data.Objects.save_object irc_connected_type { state = Connected; time = Unix.gettimeofday (); }
          >>= fun k -> Env.Data.Objects.link_to_parent account irc_account_type k irc_connected_type
        in
        let server = (Env.Data.Objects.get irc_account_type account).server in
        let%lwt chans =
          let%lwt all_chans = Env.Data.Objects.object_get_all_children account irc_channel_type in
          React.S.map (fun l ->
            Ocsigen_messages.errlog "joining channels";
            List.map (Env.Data.Objects.get irc_channel_type) l) all_chans
          |> ReactiveData.RList.from_signal
          |> RList.map (fun channel ->
            Ocsigen_messages.errlog (Format.sprintf "joining %s on server %s" channel.name server);
            let channel = channel.name in
            Irc.send_join ~connection ~channel)
          |> return
        in
        ping_server account connection
        >>= fun a ->
        let%lwt () = Env.Data.Objects.save_object irc_connected_type { state = Disconnected; time = Unix.gettimeofday (); }
          >>= fun k -> Env.Data.Objects.link_to_parent account irc_account_type k irc_connected_type
        in
        Lwt.return (a, chans)
      | None -> Lwt.return ((), ReactiveData.RList.from_signal @@ fst @@ React.S.create [])
    ) con |> RList.signal |> Lwt_react.S.keep |> return

  
  let send_to_channel channel_obj msg =
    let open Env.Data.Objects in
    let%lwt account = get_parent irc_account_type channel_obj in
    let channel = get irc_channel_type channel_obj in
    let%lwt all_connections = all_connections in
    Lwt_list.map_s (fun a -> a) (React.S.value all_connections)
    >>= Lwt_list.find_s (fun (_, l) ->
      return (Env.Data.Objects.o_eq l account)) 
    >>= fun (connection, account) ->
        match connection with
        | Some connection ->
            let%lwt msg_obj = save_object irc_message_type msg in
            let%lwt () = link_to_parent channel_obj irc_channel_type msg_obj irc_message_type in
            Irc.send_privmsg ~connection ~target:channel.name ~message:msg.content
        | None -> Lwt.return ()

  

end
