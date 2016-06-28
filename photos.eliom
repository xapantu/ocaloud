[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

    type image = {
        filename:string;
        name: Markdown_ast.file;
        alt:string;
        download_path:string list;
    }
]

open Markdown_lexer
open React
open Str_utils
  

module Photos(Env:App_stub.ENVBASE) = struct

  type album = {
    name: string;
    id: string;
    path: string;
    description: Widgets.div_content;
    volume: Env.Data.volume;
    image_list: image list React.signal;
  }
  
  exception Album_does_not_exist of string

  let albums = Hashtbl.create 10

  let album_from_id = Hashtbl.find albums

  let add_album = Hashtbl.add albums

  
  let load_images_for_album album =
    let open React.S in
    
    let extensions = [".jpg";".JPG";".png";".PNG";".jpeg";".JPEG"] in
    let volume = Env.Data.volume_from_id album in
    let album_path = Env.Data.volume_path volume in

    Env.Data.volume_list_files volume
    |> map (List.filter (fun f ->
      string_endswiths extensions Shared.(f.name)))
    |> map (List.map @@ fun f->
            let caption_path = album_path ^ "/.captions/" ^ Shared.(f.name) ^ ".md" in
            let name =
              try
                Markdown.openfile caption_path
              with | Unix.Unix_error(_) -> Markdown.empty
            in
            { filename = album_path ^ "/" ^ f.name;
              name = name;
              alt = "";
              download_path = Env.Files.download_path volume f.name;
            }
           )

  let edit_album_form =
    let album_list = Env.Data.volumes_enabled_for "photos" in
    Env.Form.(make_parametrized (string "content" "") (string_list "album" album_list Env.Data.volume_id)
      (fun album content ->
        Ocsigen_messages.errlog (Format.sprintf "setting content %s for album %s" (Env.Data.volume_id album) content);
         return ()
      ))
             
  let album_load_from_id album =
    try
      let volume = Env.Data.volume_from_id album in
      let path = Env.Data.volume_path volume in
      let index_file_data = Markdown.openfile (path ^ "/" ^ "index.md") in

      let image_list = 
        load_images_for_album album (* all images of the album *)
        |> React.S.map (List.sort (fun i j -> String.compare i.filename j.filename)) (* sorted images *)
      in
      { name = album;
        id = album;
        path = path;
        description = Markdown.to_html index_file_data;
        volume = volume;
        image_list = image_list
      }
    with
    | Unix.Unix_error(_) -> raise (Album_does_not_exist(album))

  let main_service =
    Eliom_service.App.service ~path:["p"] ~get_params:Eliom_parameter.(suffix @@ string "album") ()


  let create_display_view files_service images_list =
    [%client
      let open Html5.F in
      ~%images_list (* every image to display *)
      |> React.S.map (List.map (fun album_img -> (* convert them to html elements *)
        let img_uri = make_uri ~service:~%files_service album_img.download_path in

        let html_img = img ~src:img_uri ~alt:album_img.alt () in
        let html_descr = div [Markdown.to_html album_img.name] in
        div [Raw.a ~a:[a_href img_uri] [html_img]; html_descr] ~a:[a_class ["full-page-photo"]]
      ))
      |> React.S.map div (* pack everyone in a div *)
      |> Html5.R.node (* transform the signal into a proper html node *)
    ]
    |> Html5.C.node

  let () =
    let resize_client_on_load () =
      [%client
        let resize () =
          Js.Unsafe.eval_string "console.log(42)"
        in
        let _ = Lwt_js_events.onresizes (fun _ _ -> resize (); Lwt.return ()) in
        let%lwt _  = Lwt_js_events.domContentLoaded () in
        let _ = resize () in
        Lwt.return ()
      ]
    in
    Env.Config.App.register
      ~service:main_service
      (fun (album_id) () ->
         try%lwt
           let album = album_from_id album_id in
           let files_service = Env.Files.service_for_volume album.volume in
           let images_list = Eliom_react.S.Down.of_react album.image_list in
           let image_grid_view = create_display_view files_service images_list in
           let _ = resize_client_on_load () in
           Env.F.main_box_sidebar [album.description; image_grid_view; edit_album_form album.volume ()]
         with
         | Album_does_not_exist(_) ->
           Env.F.main_box_sidebar
             Html5.F.(
               [p [pcdata "This album does not seem to exist. Please make sure the directory exists and has an index.md file."]]
             )
      )

  let register_service_for_album album_id =
    let album = album_load_from_id album_id in
    let () = add_album album_id album in
    Eliom_service.preapply ~service:main_service album.id
    |> Env.Mimes.register_public album_id

  let () =
    Env.Data.new_volume_enabled_for "photos"
    |> E.map Env.Data.volume_id
    |> E.map register_service_for_album
    |> Lwt_react.E.keep

  let () =
    Env.Mimes.register_sidebar "photos" begin
      fun () ->
        let open Html5.F in
        
        let all_volumes =
          Env.Data.volumes_enabled_for "photos"
          |> S.map (List.map Env.Data.volume_id)
          |> Eliom_react.S.Down.of_react
        in

        let list_view:Widgets.div_content =
          [%client
            let open Html5.F in
            ~%all_volumes
            |> React.S.map begin
              fun volumes ->
                List.map (fun v ->
                  let service = ~%main_service in
                  let service:Common_client.unit_service =
                    Eliom_service.preapply ~service v
                  in
                  li [a ~service [pcdata v] ()]) volumes
                |> Widgets.F.list_view
            end
            |> Html5.R.node
          ]
          |> Html5.C.node
        in

        Lwt.return (div [h1 [pcdata "photos"]; list_view])
    end

end
