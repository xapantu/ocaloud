open Ocsipersist
open Lwt

open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

exception Not_logged_in
exception Not_allowed

let user_id_ref = 
  Eliom_reference.Volatile.eref
    ~scope:Eliom_common.default_session_scope
    None

let user_id_signal = 
  Eliom_reference.Volatile.eref
    ~scope:Eliom_common.default_session_scope
    None

let unwrap_signal () =
  match Eliom_reference.Volatile.get user_id_signal with
  | Some (s, g) -> s, g
  | None ->
    let s, g = React.S.create None in
    Eliom_reference.Volatile.set user_id_signal (Some (s, g));
    s, g

let permission_table = (open_table "permissions":string list table)
let permission_list_table = (open_table "permissions_list":unit table)

let register_permission p = 
  try%lwt
    let%lwt _ = find permission_list_table p in
    return ()
  with
  | Not_found -> add permission_list_table p ()

(* this creates an lwt thread, so it execution is not guaranteed *)
let _ = register_permission "logged"

let list_users () =
  let%lwt all_permissions = fold_table (fun s _ l -> return (s::l)) permission_list_table [] in
  fold_table (fun s p (q, l) -> return (q, ((s, p)::l))) permission_table (all_permissions, [])

let ensure_role = function
  | "" -> Lwt.return ()
  | role ->
    match Eliom_reference.Volatile.get user_id_ref with
    | None -> raise Not_allowed
    | Some login -> 
      try%lwt
        let%lwt user_permissions = find permission_table login in
        let _ = List.find (fun c -> c = role) user_permissions in
        Lwt.return ()
      with
      | Not_found -> raise Not_allowed


(* *)
let set_permission login perm value =
  let%lwt user_permissions = find permission_table login in
  if value then
    if (List.mem perm user_permissions) then return ()
    else add permission_table login (perm::user_permissions)
  else
    add permission_table login (List.filter (fun p -> p <> perm) user_permissions)

(* Save the login in the session variables, load permissions, create them if needed, etc. *)
let perform_login login =
  Eliom_reference.Volatile.set user_id_ref (Some login);
  (snd (unwrap_signal ())) (Some login);
  try%lwt
    let%lwt user_permissions = find permission_table login in
    let _ = List.find (fun c -> c = "logged") user_permissions
    in return (); 
  with
  | Not_found -> add permission_table login ["logged"]

let get_login () =
  match Eliom_reference.Volatile.get user_id_ref with
  | Some login -> login
  | _ -> raise Not_logged_in

let ensure_login () = let _ = get_login () in return ()

let login_signal () =
    fst (unwrap_signal ())

