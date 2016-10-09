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
    Store.master task store

  type object_id = { timestamp: int; device: string; sha: string; id: string; }
  type 'a object_data = bytes * object_id

  type 'a object_type = 
    { name: string;
      dec:  Protobuf.Decoder.t -> 'a;
      enc:  'a -> Protobuf.Encoder.t -> unit; }

  exception Not_implemented

  let (all_type_signals: (string, (((bytes * object_id) list React.signal) * (?step:React.step -> (bytes * object_id) list -> unit)) Lwt.t) Hashtbl.t) = Hashtbl.create 50
  let (all_children_signals: (string * string, (((bytes * object_id) list React.signal) * (?step:React.step -> (bytes * object_id) list -> unit)) Lwt.t) Hashtbl.t) = Hashtbl.create 100
  let value_store = Ocsipersist.open_store "object-manager"

  let create_object_type n d e =
    { name = n; dec = d; enc = e; }

  let get_id_as_string obj =
    (snd obj).id
  
  let get_reacts_children {id = name; _ } { name = obj_type; _ } =
    try%lwt
      Hashtbl.find all_children_signals (name, obj_type)
    with
    | Not_found ->
      (* init the signal *)
      let s =
        Ocsigen_messages.errlog "signal loading";
        repo
        >>= fun t -> Store.list (t "Getting all children of an object") ["children"; name; obj_type]
        >>= Lwt_list.map_s (fun k ->
          assert (List.length k = 4);
          let k_id = List.nth k 3 in
          let k = ["_" ^ obj_type; k_id] in
          Store.read_exn (t "Reading") k
          >|= fun d -> Bytes.to_string d, { id = k_id; timestamp = 0; sha = ""; device = "";})
        >>= fun t ->
        Lwt.return (Ocsigen_messages.errlog "signal loaded")
        >>= fun () ->
        let signal, setter = React.S.create t in
        Lwt.return (signal, setter)
      in
      Hashtbl.add all_children_signals (name, obj_type) s; s


  let get_signal_children o obj_type =
    get_reacts_children o obj_type >|= fst

  let get_setter_children o obj_type=
    get_reacts_children o obj_type >|= snd
  

  let get_reacts_for_type {name = obj_type_name; _ } =
    try%lwt
      Hashtbl.find all_type_signals obj_type_name
    with
    | Not_found ->
      Ocsigen_messages.errlog "signal loading type";
      let s = 
        repo
        >>= fun t ->
        Store.list (t "Getting all for types") ["_" ^ obj_type_name]
        >>= Lwt_list.map_s (fun k ->
          assert (List.length k = 2);
          let k_id = List.nth k 1 in
          Store.read_exn (t "Reading") k
          >|= fun d -> Bytes.to_string d, { id = k_id; timestamp = 0; sha = ""; device = "";})
        >>= fun t ->
        Lwt.return (Ocsigen_messages.errlog "signal type loaded")
        >>= fun () ->
        let signal, setter = React.S.create t in
        Lwt.return (signal, setter)
      in
      Hashtbl.add all_type_signals obj_type_name s; s

  let get_signal_for_type o =
    get_reacts_for_type o >|= fst

  let get_setter_for_type o =
    get_reacts_for_type o >|= snd
  
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
    repo >>=  fun t ->
    Store.update (t (Format.sprintf "Adding %s" id.id)) ["_" ^ obj_type.name; id.id] data_encoded
    >>= fun () ->
    let%lwt setter = get_setter_for_type obj_type in
    let%lwt signal = get_signal_for_type obj_type in
    setter ((data_encoded, id) :: React.S.value signal);
    Lwt.return ()
    >>= fun () -> Lwt.return (data_encoded, id)

  let link_to_parent (_, a) obj_type_parent (db, b) obj_type =
    repo
    >>= fun t ->
    Store.update (t "Linking") ["children"; a.id; obj_type.name; b.id] ""
    >>= fun () ->
    Store.update (t "Linking") ["parent"; b.id; obj_type_parent.name; a.id] ""
    >>= fun () ->
    (* FIXME: check for duplicates *)
    let%lwt setter = get_setter_children a obj_type in
    let%lwt signal = get_signal_children a obj_type in
    setter ((db, b) :: React.S.value signal);
    Lwt.return_unit

  let get_parent obj_type child =
    repo
    >>= fun t -> Store.list (t "Getting all parent of an object") ["parent"; (snd child).id; obj_type.name]
    >>= fun k ->
      assert (List.length k >= 1);
      let p = List.nth k 0 in
      assert (List.length p >= 4);
      let p_id = List.nth p 3 in
      let%lwt s = Store.read_exn (t "reading parent") ["_" ^ obj_type.name; p_id] in
      Lwt.return (Bytes.to_string s, { id = p_id;
                       timestamp = -1;
                       device = "";
                       sha = ""; })

  let get_object_of_type obj_type =
    get_signal_for_type obj_type

  let object_get_all_children (_, id)  obj_type =
    get_signal_children id obj_type

  let o_eq (_, id) (_, id2) =
    id.id = id2.id

end
