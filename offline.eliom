let down_of_react = Eliom_react.S.Down.of_react

let time_update_signal, do_time_update = React.S.create 0.0

let _ =
  let t = Unix.time () in
  while%lwt true do
    let%lwt _ = Lwt_unix.sleep 30.0 in
    do_time_update (Unix.time () -. t);
    Lwt.return_unit
  done

let time_update_signal = down_of_react time_update_signal

[%%client

  let time_update_signal_client, do_time_update_client = React.S.create (((new%js Js.date_now)##getTime)/.1000.)

  let _ =
    while%lwt true do
      let%lwt _ = Lwt_js.sleep 30.0 in
      let d = new%js Js.date_now in
      let f = (d##getTime)/.1000. in
      do_time_update_client f;
      Lwt.return_unit
    done

  let is_connected =
    let init_server = React.S.value ~%time_update_signal in
    let init_client = React.S.value time_update_signal_client in
    React.S.l2 (fun fs fc ->
      let diff = abs_float @@  (fs -. fc) -. (init_server -. init_client)
      in
      diff < 90.) ~%time_update_signal time_update_signal_client


  let _ = React.S.map (fun b ->
    if not b then
      Js.Unsafe.eval_string @@ "alert('Network disconnected.')";) is_connected
          |> Lwt_react.S.keep

  open Lwt
  let persistent_of_react n a = a

  let _ =
    let%lwt _ = Lwt_js_events.onload () in
    let open Dom_html in
    let open Dom in
    let span = createSpan document in
    let head = getElementById "top-header" in
    let () = appendChild head span in
    let t = document##createTextNode (Js.string "online") in
    let () = appendChild span t in
   React.S.map (fun b ->
      if b then
        t##replaceData 0 1000 (Js.string "online")
      else
        t##replaceData 0 1000 (Js.string "offline");
      Lwt.return ()
  ) is_connected |> Lwt_react.S.keep |> Lwt.return

  let if_online: (unit -> 'a React.signal) -> 'a -> 'a React.signal = fun  a b ->
    let getter, setter = React.S.create [] in
    Lwt.async (fun () ->
      let result = a () in
      React.S.map setter result |> Lwt_react.S.keep |> return
      );
    setter b; getter
]
