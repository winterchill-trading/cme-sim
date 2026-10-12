(** Reading and writing SOFH-framed messages (iLink 3 messages, MDP packets over TCP) on
    Async readers and writers. *)

open! Core
open! Async

(** Reads one whole frame, SOFH included. Fails on a SOFH with the wrong encoding type. *)
val read : Reader.t -> Bigstring.t Reader.Read_result.t Deferred.Or_error.t

val write : Writer.t -> Bigstring.t -> unit
