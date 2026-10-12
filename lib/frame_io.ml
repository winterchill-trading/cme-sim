open! Core
open! Async
module Sbe = Cme_sim_wire.Sbe

let read reader =
  let header = Bytes.create Sbe.Sofh.size in
  match%bind Reader.really_read reader header with
  | `Eof _ -> return (Ok `Eof)
  | `Ok ->
    let header = Bigstring.of_bytes header in
    (match Sbe.Sofh.read header ~pos:0 with
     | Error _ as e -> return e
     | Ok size ->
       let body = Bytes.create (size - Sbe.Sofh.size) in
       (match%map Reader.really_read reader body with
        | `Eof _ -> Ok `Eof
        | `Ok ->
          let frame = Bigstring.create size in
          Bigstring.blit ~src:header ~src_pos:0 ~dst:frame ~dst_pos:0 ~len:Sbe.Sofh.size;
          Bigstring.From_bytes.blit
            ~src:body
            ~src_pos:0
            ~dst:frame
            ~dst_pos:Sbe.Sofh.size
            ~len:(Bytes.length body);
          Ok (`Ok frame)))
;;

let write writer frame = Writer.write_bigstring writer frame
