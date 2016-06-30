[%%shared
    open Eliom_content
    open Lwt

    type div_content = Html5_types.div_content_fun Eliom_content.Html5.elt
  

    module F = struct

      let list_view l =
        Html5.F.ul ~a:[Html5.F.a_class ["list-view"]] l

      let two_panes (child1:div_content) (child2:div_content) =
        let open Html5.F in
        let open Html5 in
        Html5.F.(div ~a:[a_class ["two-panes"]] [ div [child1]; div [child2]])

    end]

[%%client
    module C = struct
      let link callback l = 
        let open Html5.F in
        let open Html5 in
        let button  = span ~a:[a_class ["link"]; a_onclick (fun e -> callback (); ())] l in
        button

    end
]

(* this must be in a separate module, as it can not be put in client's code *)
module S (M: App_stub.MIMES) = struct
  let main_box l =
    let open Html5.F in
    return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~css:[["css";"ocaloud.css"]]
      (body l))

  let main_box_sidebar l =
    let open Html5.F in
    let%lwt sidebar =
      M.build_sidebar ()
      >>= fun a ->
      return @@ Html5.F.div ~a:[a_class ["sidebar"]] a
    in
    let main_wrapper = Html5.F.div ~a:[a_class ["main-wrapper"]]
        l in
    Lwt.return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~css:[["css";"ocaloud.css"]]
      (body ([sidebar; main_wrapper])))

  let flex_box_sidebar l =
    let open Html5.F in
    let%lwt sidebar =
      M.build_sidebar ()
      >>= fun a ->
      return @@ Html5.F.div ~a:[a_class ["sidebar"]] a
    in
    let main_wrapper = Html5.F.div ~a:[a_class ["flex-wrapper"]]
        l in
    Lwt.return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~css:[["css";"ocaloud.css"]]
      (body ([sidebar; main_wrapper])))

end

