let down_of_react = Eliom_react.S.Down.of_react

[%%client
  open Lwt
  let persistent_of_react n a = a

  let is_connected, is_connected_set = React.S.create false

  let is_connected_label =
    let%lwt _ = Lwt_js_events.onload () in
    let open Dom_html in
    let open Dom in
    let span = createSpan document in
    let head = getElementById "top-header" in
    let () = appendChild head span in
    let t = document##createTextNode (Js.string "offline") in
    let () = appendChild span t in
    Lwt.return t

  let _ =
    React.S.map (fun b ->
      let%lwt t = is_connected_label in
      if b then
        t##replaceData 0 1000 (Js.string "online")
      else
        t##replaceData 0 1000 (Js.string "offline");
      Lwt.return ()
    ) is_connected |> Lwt_react.S.keep

  let if_online: (unit -> 'a React.signal) -> 'a -> 'a React.signal = fun  a b ->
    let getter, setter = React.S.create [] in
    Lwt.async (fun () ->
      let result = a () in
      is_connected_set true;
      React.S.map setter result |> Lwt_react.S.keep |> return
      );
    setter b; getter
]
