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
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Eliom_tools.D
  let forall_head = [meta
                     ~a:[a_name "viewport";
                         a_content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width, height=device-height, target-densitydpi=device-dpi"]
                     (); ]
  let main_box l =
    let open Html5.F in
    return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~js:[["js"; "app.js"]]
      ~css:[["css";"ocaloud.css"]]
      ~other_head:forall_head
      (body l))

  let top_bar () =
    let button_menu = Html5.D.Raw.a [pcdata " "] in
    let button_back = Html5.D.Raw.a [pcdata " "] in
    let _ =	[%client
      let a = Eliom_content.Html5.To_dom.of_a ~%button_back in
      let menus = Dom_html.getElementById "main" in
      let sliding_content = Dom_html.getElementById "sliding-content" in
      Lwt_js_events.clicks a (fun e _ ->
        Dom.preventDefault e;
        menus##.classList##remove (Js.string "activated");
        sliding_content##.classList##add (Js.string "activated");
        Lwt.return ());
      let a = Eliom_content.Html5.To_dom.of_a ~%button_menu in
      Lwt_js_events.clicks a (fun e _ ->
        Dom.preventDefault e;
        sliding_content##.classList##remove (Js.string "activated");
        menus##.classList##add (Js.string "activated");
        Lwt.return ()) ] in
    Html5.F.div ~a:[a_id "top-header"] [button_menu; button_back]

  let main_box_sidebar l =
    let open Html5.F in
    let%lwt sidebar =
      M.build_sidebar ()
      >>= fun a ->
      return @@ Html5.F.div ~a:[a_id "sidebar"] a
    in
    let sidebar = Html5.F.div ~a:[a_id "main"] [sidebar] in
    let main_wrapper = Html5.F.div ~a:[a_class ["main-wrapper"]]
        l in
    let sliding_content = Html5.F.div ~a:[a_id "sliding-content"; a_class ["activated"]] [main_wrapper;] in
    Lwt.return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~js:[["js"; "app.js"]]
      ~css:[["css";"ocaloud.css"]]
      ~other_head:forall_head
      (body ([top_bar (); sidebar; sliding_content])))

  let flex_box_sidebar l =
    let open Html5.F in
    let%lwt sidebar =
      M.build_sidebar ()
      >>= fun a ->
      return @@ Html5.F.div ~a:[a_id "sidebar"] a
    in
    let sidebar = Html5.F.div ~a:[a_id "main"] [sidebar] in
    let main_wrapper = Html5.F.div ~a:[a_class ["flex-wrapper"]]
        l in
    let sliding_content = Html5.F.div ~a:[a_id "sliding-content"; a_class ["activated"]] [main_wrapper;] in
    Lwt.return (Eliom_tools.F.html
      ~title:"ocaloud"
      ~js:[["js"; "app.js"]]
      ~css:[["css";"ocaloud.css"]]
      ~other_head:forall_head
      (body ([top_bar (); sidebar; sliding_content])))

  let make_page_redirect url l =
    Lwt.return (html
      ~title:"restricted area"
      ~js:[["js";"sjcl.js"]]
      ~css:[["css";"main.css"]]
      ~other_head:((meta ~a:[a_http_equiv "refresh"; a_content ("1;url="^url) ] ())::forall_head)
      (body l
      ))

end

