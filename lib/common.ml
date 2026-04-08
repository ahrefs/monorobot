open Devkit

let ( let* ) = Lwt_result.bind

module Slack_timestamp = Fresh (String) ()

module Timestamp = struct
  type t = Ptime.t

  let wrap s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> t
    | Error _ -> failwith "Invalid timestamp"
  let unwrap t = Ptime.to_rfc3339 t

  let wrap_with_fallback ?(fallback = Ptime_clock.now ()) s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> t
    | Error _ -> fallback
end

module Slack_channel : sig
  type 'kind t

  val equal : 'kind t -> 'kind t -> bool
  val compare : 'kind t -> 'kind t -> int
  val hash : 'kind t -> int

  module Ident : sig
    type nonrec t = [ `Id ] t
    val inject : string -> t
    val project : t -> string
  end
  module Name : sig
    type nonrec t = [ `Name ] t
    val inject : string -> t
    val project : t -> string
  end
  module Any : sig
    type nonrec t = [ `Any ] t
    val inject : string -> t
    val project : t -> string
  end

  val to_any : 'kind t -> Any.t
end = struct
  type 'kind t = string

  let equal = String.equal
  let compare = String.compare
  let hash = Hashtbl.hash
  let to_any = id

  module Ident = struct
    type nonrec t = [ `Id ] t
    let inject = id
    let project = id
  end
  module Name = struct
    type nonrec t = [ `Name ] t
    let inject = id
    let project = id
  end
  module Any = struct
    type nonrec t = [ `Any ] t
    let inject = id
    let project = id
  end
end

module Slack_user_id = struct
  include Fresh (String) ()

  let to_channel_id = Slack_channel.Any.inject $ project
end

module Set (S : Set.OrderedType) = struct
  include Set.Make (S)

  let wrap = of_list
  let unwrap = elements
end

module StringSet = Set (String)
module FailedStepSet = Set (struct
  type t = Buildkite_t.failed_step
  let compare (t1 : t) (t2 : t) = String.compare t1.id t2.id
end)

module Status_notification = struct
  type t =
    | Channel of Slack_channel.Any.t
    | User of Slack_user_id.t

  let inject_channel c = Channel (Slack_channel.to_any c)

  let to_slack_channel = function
    | Channel c -> c
    | User u -> Slack_user_id.to_channel_id u

  let is_user = function
    | User _ -> true
    | Channel _ -> false

  let compare a b = Slack_channel.compare (to_slack_channel a) (to_slack_channel b)
  let equal a b = compare a b = 0
end

module Map (S : Map.OrderedType) = struct
  include Map.Make (S)

  let to_list (l : 'a t) : (S.t * 'a) list = to_seq l |> List.of_seq
  let of_list (m : (S.t * 'a) list) : 'a t = List.to_seq m |> of_seq
  let wrap = of_list
  let unwrap = to_list

  let of_list_multi (m : (S.t * 'a) list) : 'a list t =
    let update_f v = function
      | None -> Some [ v ]
      | Some vs -> Some (v :: vs)
    in
    List.fold_right (fun (k, v) b -> update k (update_f v) b) m empty
end

module StringMap = struct
  include Map (String)

  (** [update_async key f map] updates the value of [key] in [map] using an async function [f].
      Async equivalent of the [update] function. *)
  let update_async key f map =
    let current_value = find_opt key map in
    match%lwt f current_value with
    | None -> Lwt.return (remove key map)
    | Some v -> Lwt.return (add key v map)
end

module IntMap = Map (Int)

module IntMapJson = struct
  type 'a t = 'a IntMap.t

  let to_list (m : 'a t) : (string * 'a) list =
    IntMap.to_seq m |> Seq.map (fun (k, v) -> string_of_int k, v) |> List.of_seq

  let of_list (l : (string * 'a) list) : 'a t =
    List.to_seq l |> Seq.map (fun (k, v) -> int_of_string k, v) |> IntMap.of_seq

  let wrap = of_list
  let unwrap = to_list
end

module ChannelMap = Map (struct
  include Slack_channel.Any
  let compare = Slack_channel.compare
end)

module Stringtbl = struct
  include Hashtbl

  type 'a t = (string, 'a) Hashtbl.t

  let empty () = Hashtbl.create 1
  let to_list (l : 'a t) : (string * 'a) list = Hashtbl.to_seq l |> List.of_seq
  let of_list (m : (string * 'a) list) : 'a t = List.to_seq m |> Hashtbl.of_seq
  let wrap = of_list
  let unwrap = to_list
end

module Regex = struct
  type t = string * Re.re

  let wrap s = s, Re.Perl.compile_pat s
  let unwrap (s, _) = s

  let matches (_, re) s = Re.execp re s
end
