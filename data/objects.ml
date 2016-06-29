open Lwt_react
open Lwt


module Object_manager = struct

  type object_id = { timestamp: int; device: string; sha: string; id: string; }
  type 'a object_data = bytes * object_id

  type 'a object_type = string * (Protobuf.Decoder.t -> 'a) * ('a -> Protobuf.Encoder.t -> unit)

  exception Not_implemented

  let value_store = Ocsipersist.open_store "object-manager"

  let all_signals = Hashtbl.create 50
  let all_child_signals = Hashtbl.create 50

  let links_table = Ocsipersist.open_table "_links"

  let get_id_as_string obj =
    (snd obj).id

  let fresh_id () =
    let%lwt myid = Ocsipersist.make_persistent value_store "ids" 1 in
    let%lwt current_id = Ocsipersist.get myid in
    let%lwt () = Ocsipersist.set myid (current_id + 1) in
    Lwt.return { timestamp = int_of_float @@ Unix.time ();
                 device = "mydevice";
                 sha = "mysha";
                 id = string_of_int current_id; }

  let get (name, dec, enc) (b, _) =
    Protobuf.Decoder.decode_exn dec b

  let get_signal name =
    Lwt.return @@ try
      fst @@ Hashtbl.find all_signals name
    with
    | Not_found ->
      let table = Ocsipersist.open_table name in
      let signal, signal_setter = S.create [] in
      let _ = 
        let%lwt all_objects = Ocsipersist.fold_step (fun n obj q ->
          Lwt.return (obj :: q)) table [] in
        Lwt.return (signal_setter all_objects)
      in
      Lwt_react.S.keep signal;
      Hashtbl.add all_signals name (signal, signal_setter);
      signal

  let get_signal_setter name =
    let%lwt _ = get_signal name in
    Lwt.return @@ snd @@ Hashtbl.find all_signals name

  let get_child_signal obj =
    try
      Lwt.return @@ fst @@ Hashtbl.find all_child_signals obj
    with
    | Not_found ->
      let signal, signal_setter = S.create [] in
      let _ = 
        let%lwt all_objects = Ocsipersist.fold_step (fun n (a, b) q ->
          if a = obj.id then
            Lwt.return (b :: q)
          else Lwt.return q) links_table [] in
        Lwt.return (signal_setter all_objects)
      in
      Hashtbl.add all_child_signals obj (signal, signal_setter);
      Lwt_react.S.keep signal;
      Lwt.return signal

  let get_child_signal_setter name =
    let%lwt _ = get_child_signal name in
    Lwt.return @@ snd @@ Hashtbl.find all_child_signals name
  

  let (open_type_table:string -> (bytes * object_id) Ocsipersist.table) = fun name ->
    Ocsipersist.open_table name

  let save_object (obj_type, dec, enc) data =
    let%lwt id = fresh_id () in
    let data_encoded = Protobuf.Encoder.encode_exn enc data in
    let table = open_type_table obj_type in
    let%lwt () = Ocsipersist.add table id.id (data_encoded, id) in
    let%lwt setter = get_signal_setter obj_type in
    let%lwt signal = get_signal obj_type in
    let () = setter ((data_encoded, id) :: (S.value signal)) in
    Lwt.return (data_encoded, id)

  let link_to_parent (_, a) (_, b) =
    let%lwt () = Ocsipersist.add links_table (a.id ^ "|" ^ b.id) (a.id, b.id) in
    let%lwt current_children = get_child_signal a in
    let%lwt setter = get_child_signal_setter a in
    return @@ setter (b.id :: S.value current_children)

  let get_parent (obj_type, _, _) child =
    let unwrap = function
      | None -> failwith "No parent for this object"
      | Some s -> s
    in
    let%lwt parent_id = Ocsipersist.fold_step (fun n (a, b) s ->
      if b = (snd child).id then
        return (Some a)
      else
        return s) links_table None
    in
    let parent_id = unwrap parent_id in
    let obj_table = open_type_table obj_type in
    Ocsipersist.find obj_table parent_id


  let get_object_of_type (obj_type, dec, enc) =
    get_signal obj_type

    
  let object_is_outdated _ =
    raise Not_implemented

  let object_get_all_children (_, id)  (obj_type, dec, enc) =
    let obj_table = open_type_table obj_type in
    get_child_signal id
    >>= S.map_s @@ Lwt_list.map_s (fun id ->
      let%lwt obj = Ocsipersist.find obj_table id in
      Lwt.return obj
    )

end
