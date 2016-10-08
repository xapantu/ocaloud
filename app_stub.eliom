open Lwt_react

[%%shared
type div_content = Html5_types.div_content_fun Eliom_content.Html5.elt
type action_on_form = Clear | Nothing

]

module type CONFIG = sig
  module App: Eliom_registration.ELIOM_APPL
end

module type DEVICES = sig
  type device
  val list_devices: unit -> device list
  val new_device : device event
  val all_devices : (string list) signal
  val name : device -> string
  val new_device: string -> unit
end


module type DATA = sig
  type volume

  module Devices : DEVICES
  module Objects : sig
    (* first one is the concrete type, second one the internal one *)
    type 'a object_type
    type _ object_data
    val create_object_type: string -> (Protobuf.Decoder.t -> 'a) -> ('a -> Protobuf.Encoder.t -> unit) -> 'a object_type
    val save_object: 'a object_type -> 'a -> 'a object_data Lwt.t
    val link_to_parent: 'a object_data -> 'b object_data -> unit Lwt.t
    val get_object_of_type: 'a object_type -> 'a object_data list signal Lwt.t
    val object_get_all_children: 'c object_data -> 'a object_type -> 'a object_data list signal Lwt.t
    val get_parent: 'a object_type -> 'c object_data -> 'a object_data Lwt.t
    val get: 'a object_type -> 'a object_data -> 'a
    val get_id_as_string: 'a object_data -> string
  end


  open Devices

  val all_volumes : volume list signal
  val volumes_enabled_for : string -> volume list signal
  val new_volume_enabled_for : string -> volume event
  val volume_id : volume -> string
  val public_volume : volume -> bool
  val volume_path : volume -> string
  val volume_from_id : string -> volume

  val load_volumes : unit -> unit Lwt.t

  val new_volume_loaded : volume React.event

  val volume_list_files : volume -> Shared.file list signal

  val volume_sync_for_device : volume -> device -> float React.event

  exception Volume_not_found of string

end

type unit_service = (unit, unit, Eliom_service.get_service_kind, Eliom_service.attached,
                     Eliom_service.service_kind, [ `WithoutSuffix ], unit, 
                     unit, Eliom_service.registrable, [ `Appl ])
    Eliom_service.service 

[%%shared
type path_service =
  (string list, unit, Eliom_service.get_service_kind,
   Eliom_service.attached,
   Eliom_service.internal_service_kind, [ `WithSuffix ],
   [ `One of string list ] Eliom_parameter.param_name, 
   unit, Eliom_service.registrable,
   [ `Appl ])
    Eliom_service.service
]

module type MIMES = sig
  val register_public : string -> unit_service-> unit
  val get_all_public_services : unit -> (string * unit_service) list
  val register_sidebar : string -> (unit -> Html5_types.div Eliom_content.Html5.elt Lwt.t) -> unit
  val build_sidebar : unit -> Html5_types.div Eliom_content.Html5.elt list Lwt.t

end

module type ENV = sig
  module Config : CONFIG
  module Data : DATA
  module Mimes : MIMES
  module F : sig
    val main_box : (div_content list) -> Html5_types.html Eliom_content.Html5.elt Lwt.t
    val main_box_sidebar : (div_content list) -> Html5_types.html Eliom_content.Html5.elt Lwt.t
    val flex_box_sidebar : (div_content list) -> Html5_types.html Eliom_content.Html5.elt Lwt.t
    val make_page_redirect: string -> (div_content list) -> Html5_types.html Eliom_content.Html5.elt Lwt.t
  end
  module Form : sig
    (* the first one is the actual datatype, the second one the serialized one - they are equal for simple datatype,
     * it gets complicated for string list *)
    type ('a, 'b) params_type
    val string: string -> string -> (string, string) params_type
    val string_password: string -> string -> (string, string) params_type
    val string_list: string -> 'a list signal -> ('a -> string) -> ('a, string) params_type
    val int: string -> int -> (int, int) params_type
    val bool: string -> bool -> (bool, bool) params_type
    val ( ** ): ('a, 'b) params_type -> ('c, 'd) params_type -> ('a * 'c, 'b * 'd) params_type
    val make: ('a, 'b) params_type -> ('a -> 'c Lwt.t) -> ('c -> action_on_form) Eliom_lib.client_value option -> unit -> div_content
    (** a parametrized form is a form whose some fields are hidden *)
    val make_parametrized: ('a, 'b) params_type -> ('c, 'd) params_type ->  ('c -> 'a -> 'e Lwt.t) -> ('e -> action_on_form) Eliom_lib.client_value option -> 'c ->  unit -> div_content
  end
end

module type FILES = sig
  type volume
  val service_for_volume: volume -> path_service
  val download_path: volume -> string -> string list
end

module type ENVBASE = sig
  include ENV
  module Files : FILES with type volume = Data.volume
  module Permissions : sig
    val ensure_role: string -> (unit -> Html5_types.html Eliom_content.Html5.elt Lwt.t) -> Html5_types.html Eliom_content.Html5.elt Lwt.t
  end
  val welcome_service: unit_service
end

