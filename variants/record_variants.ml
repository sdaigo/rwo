open Base
module Time_ns = Core.Time_ns

module Log_entry = struct
  type t = { important : bool; message : string }
end

module Heartbeat = struct
  type t = { status_message : string }
end

module Logon = struct
  type t = { user : string; credentials : string }
end

module Common = struct
  (* メッセージ共通のフィールド *)
  type t = { session_id : string; time : Time_ns.t }
end

type details =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t

(* メッセージのリストを受け取り
   特定のユーザによって生成されたメッセージを返す *)
(* string -> (Common.t * details) list -> (Common.t * details) list *)
let message_for_user user (messages : (Common.t * details) list) =
  let user_messages, _ =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(fun
          ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon d ->
            if String.( = ) d.user user then
              (message :: messages, Set.add user_sessions common.session_id)
            else acc
        | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then
              (message :: messages, user_sessions)
            else acc)
  in
  List.rev user_messages
