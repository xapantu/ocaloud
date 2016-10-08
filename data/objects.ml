open Lwt_react
open Lwt


module Object_manager = struct
  open Irmin_unix
  module Store =
    Irmin_git.FS (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  let config = Irmin_git.config ~root:"_run/irmin-store" ~bare:true ()

  let repo =
    Ocsigen_messages.accesslog "Opening objects store…";
    let%lwt store = Store.Repo.create config in
    Ocsigen_messages.accesslog "Objects store opened.";
    Lwt.return store


  type object_id = { timestamp: int; device: string; sha: string; id: string; }
  type 'a object_data = bytes * object_id

  type 'a object_type = 
    { name: string;
      dec:  Protobuf.Decoder.t -> 'a;
      enc:  'a -> Protobuf.Encoder.t -> unit; }

  exception Not_implemented

  let (all_type_signals: (string, ((bytes * object_id) list React.signal) * (?step:React.step -> (bytes * object_id) list -> unit)) Hashtbl.t) = Hashtbl.create 50
  let (all_children_signals: (string * string, ((bytes * object_id) list React.signal) * (?step:React.step -> (bytes * object_id) list -> unit)) Hashtbl.t) = Hashtbl.create 100
  let value_store = Ocsipersist.open_store "object-manager"

  let create_object_type n d e =
    { name = n; dec = d; enc = e; }

  let get_id_as_string obj =
    (snd obj).id
  
  let get_reacts_children {id = name; _ } { name = obj_type; _ } =
    try
      Hashtbl.find all_children_signals (name, obj_type)
    with
    | Not_found ->
      let signal, setter = React.S.create [] in
      Hashtbl.add all_children_signals (name, obj_type) (signal, setter);
      signal, setter

  let get_signal_children o =
    fst (get_reacts_children o)

  let get_setter_children o =
    snd (get_reacts_children o)
  

  let get_reacts_for_type {name = obj_type_name; _ } =
    try
      Hashtbl.find all_type_signals obj_type_name
    with
    | Not_found ->
      let signal, setter = React.S.create [] in
      Hashtbl.add all_type_signals obj_type_name (signal, setter);
      signal, setter

  let get_signal_for_type o =
    fst (get_reacts_for_type o)

  let get_setter_for_type o =
    snd (get_reacts_for_type o)
  
  let fresh_id () =
    let%lwt myid = Ocsipersist.make_persistent value_store "ids" 1 in
    let%lwt current_id = Ocsipersist.get myid in
    let%lwt () = Ocsipersist.set myid (current_id + 1) in
    Lwt.return { timestamp = int_of_float @@ Unix.time ();
                 device = "mydevice";
                 sha = "mysha";
                 id = string_of_int current_id; }

  let get obj_type (b, _) =
    Protobuf.Decoder.decode_exn obj_type.dec b

  let save_object obj_type data =
    let%lwt id = fresh_id () in
    let data_encoded = Protobuf.Encoder.encode_exn obj_type.enc data in
    repo >>= Store.master task >>= fun t ->
    Store.update (t (Format.sprintf "Adding %s" id.id)) ["_" ^ obj_type.name; id.id] data_encoded
    >>= fun () ->
      (get_setter_for_type obj_type) ((data_encoded, id) :: (React.S.value (get_signal_for_type obj_type)))
      |> Lwt.return
    >>= fun () -> Lwt.return (data_encoded, id)

  let link_to_parent (_, a) (_, b) =
    repo >>= Store.master task
    >>= fun t ->
    Store.update (t "Linking") ["children"; a; b] ""
    >>= fun () ->
      (get_setter_children a

  let get_parent obj_type child =
    raise Not_implemented

  let get_object_of_type obj_type =
    repo
    >>= Store.master task
    >>= fun t -> Store.list (t "Getting all for types") ["_" ^ obj_type.name]
    >>= Lwt_list.map_s (fun k ->
      assert (List.length k = 2);
      Store.read_exn (t "Reading") k
      >|= fun d -> Bytes.to_string d, { id = List.nth k 1; timestamp = 0; sha = ""; device = "";})
    >|= get_setter_for_type obj_type
    >|= fun () -> get_signal_for_type obj_type

  let object_get_all_children (_, id)  obj_type =
    raise Not_implemented

end
