type t =
  | Push
  | Pull_request
  | Check_suite
  | No_event

let from_string = function
  | "push" -> Push
  | "pull_request" -> Pull_request
  | "check_suite" -> Check_suite
  | _ -> No_event
