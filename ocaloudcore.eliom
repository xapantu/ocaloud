[%%shared
    open Eliom_lib
    open Eliom_content
]

open Config
open Data
open Mimes
open User
open Photos
open Files
open Welcome
open Irc
let _ =  Eliom_service.register_eliom_module "ocaloudcore" (fun () ->

  let module Ocaloudcore_app =
    Eliom_registration.App (
    struct
      let application_name = "ocaloudcore"
    end) in

  let module Config = Config(Ocaloudcore_app) in
  let module Data = Data.Volume_manager.VolumeManager(Config) in


  (* Apps can register to read, write, use a filetype, and ask for apps
   * that can do those things. *)
  let module Mimes = Mimes(Config) in
  (*module User = User(Data)*)


  let module Env = struct
    module Mimes = Mimes
    module Config = Config
    module Data = Data
    module F = Widgets.S(Mimes)
    module Form = Myform.Form(Data)
  end in

  let module Welcome = Welcome(Env) in

  let module Files = Files(Env) in
  let module Permissions = User.Permissions(Env) in

  let module EnvBase = struct
    include Env
    module Files = Files
    module Permissions = Permissions
    let welcome_service = Welcome.main_service
  end in

  let module Photos = Photos(EnvBase) in
  let module Irc  = IrcApp(EnvBase) in

  let _ = Data.load_volumes () in

  let main_service =
    Eliom_service.App.service ~path:["main"] ~get_params:Eliom_parameter.unit () in

  let () =
    Config.App.register
      ~service:main_service
      (fun () () ->
         Lwt.return
           (Eliom_tools.F.html
              ~title:"reactivenodes"
              ~css:[["css"; "reactivenodes.css"]]
              Html5.F.(body [p [pcdata "ocaloud v1"]])
           )) in

(*
let manifest =
  Eliom_registration.Any.register
    ~service:(Eliom_service.Http.service ~path:["manifest.appcache"] ~get_params:Eliom_parameter.unit ())
    (fun () () ->
       let appcache =
         Format.sprintf
"CACHE MANIFEST
# %d
/
css/ocaloud.css
" 0 (*(Int64.to_int @@ Eliom_request_info.get_request_id ())*)
       in
       Eliom_registration.String.send ~headers:Http_headers.(add (name "Cache-Control")  "max-age=100" empty) (appcache, ""))*)


  let () = Mimes.register_public "main" main_service in
  ()
)

(* let _ = Bep.Main.start_syncing ()*)
[%%client
    open Files
    open Photos
    open Irc
    open Myform
    open Offline
    open Welcome
    open Dumb_password
    open Mimes
]
