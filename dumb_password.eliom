open Lwt
open Config
open Ocsigen_messages
open Eliom_lib
open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

open Ocsipersist

module Dumb_password(E:App_stub.ENV) = struct

  exception BadPassword

  let send_error str =
    span [pcdata ("Error: " ^ str)]

  let password_table = (open_table "users":string table)

  let main_widget =
    E.Form.(make (string "username" "" ** string "password" "")
              (fun (user, password) ->
                 try%lwt
                   let%lwt real_password = find password_table user in
                   if real_password = password then
                     let%lwt () = User.perform_login user in
                     Lwt.return true
                   else raise BadPassword
                 with
                 | Not_found | BadPassword -> Lwt.return false)
              (Some ([%client fun b ->
                if not b then Js.Unsafe.eval_string "alert('bad password')"])))
end
