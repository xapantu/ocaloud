let down_of_react = Eliom_react.S.Down.of_react

let time_update_signal, do_time_update = React.S.create 0.0

let offline_time = 5.

let _ =
  let t = Unix.time () in
  while%lwt true do
    let%lwt _ = Lwt_unix.sleep offline_time in
    do_time_update (Unix.time () -. t);
    Lwt.return_unit
  done

let time_update_signal = down_of_react time_update_signal

[%%client

  let time_update_signal_client, do_time_update_client = React.S.create (((new%js Js.date_now)##getTime)/.1000.)

  let _ =
    while%lwt true do
      let%lwt _ = Lwt_js.sleep ~%offline_time in
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
      diff < 4. *. ~%offline_time) ~%time_update_signal time_update_signal_client


  (*let _ = React.S.map (fun b ->
    if not b then
      Js.Unsafe.eval_string @@ "alert('Network disconnected.')";) is_connected
          |> Lwt_react.S.keep*)

  open Lwt
  let persistent_of_react n a = a

  let if_online: (unit -> 'a React.signal) -> 'a -> 'a React.signal = fun  a b ->
    let getter, setter = React.S.create [] in
    Lwt.async (fun () ->
      let result = a () in
      React.S.map setter result |> Lwt_react.S.keep |> return
      );
    setter b; getter
]
