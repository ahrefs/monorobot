open Devkit

module Slack = struct
  (** identifier of a user in a Slack workspace, of form UXXXXXX *)
  module User_id = struct
    include Fresh (String) ()

    let wrap = inject
    let unwrap = project
    let is_slackbot s = String.equal (project s) "USLACKBOT"
  end
end
