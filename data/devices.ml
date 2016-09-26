open Eliom_shared.React

module Devices = struct
  type device = string

  let list_devices () =
    ["xaptop"]

  let new_device, send_device = React.E.create ()

  let all_devices = React.S.accum (React.E.map (fun a l -> a::l) new_device) []

  let () = send_device "xaptop"

  let () = send_device "testttt"

  let name n = n

  let new_device n = send_device n
end
