let down_of_react = Eliom_react.S.Down.of_react

[%%client
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
