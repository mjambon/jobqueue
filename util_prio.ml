(*
   Priority of a job.

   This can be set either at the process level or for specific
   API endpoints.

   We use this property to determine the priority of certain actions
   over others, including:
   - Gmail API call throttling: low-priority requests have to wait for the
     high-priority requests to complete.
   - Error reporting: errors occurring in a high-priority context
     (interactive use the Esper API) are more serious than in a low-priority
     context (retriable background jobs).
*)


let key = Lwt.new_key ()

let default_high_priority = ref false

let is_high_priority_job () =
  match Lwt.get key with
  | None -> !default_high_priority
  | Some true -> true
  | Some false -> false

let high_priority_job f =
  Lwt.with_value key (Some true) f

let low_priority_job f =
  Lwt.with_value key (Some false) f
