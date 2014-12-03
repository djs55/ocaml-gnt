(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Gnt
open Gnt
open Sexplib.Std
open Lwt

type 'a io = 'a Lwt.t

type grant = int32 with sexp

let grant_of_int32 x = x
let int32_of_grant x = x

let grants_of_share x = List.map Int32.of_int x.Gntshr.refs
let buf_of_share x = x.Gntshr.mapping

let open_gntshr =
  let cache = ref None in
  fun () -> match !cache with
    | Some x -> x
    | None ->
      let i = Gnt.Gntshr.interface_open () in
      cache := Some i;
      i

type share = Gntshr.share with sexp_of

let share ~domid ~npages ~rw =
  match Gntshr.share_pages (open_gntshr ()) domid npages rw with
  | None ->
    fail (Failure (Printf.sprintf "share_pages domid=%d npages=%d rw=%b" domid npages rw))
  | Some s ->
    return s

let unshare share =
  Gntshr.munmap_exn (open_gntshr ()) share;
  return ()

let open_gnttab =
  let cache = ref None in
  fun () -> match !cache with
    | Some x -> x
    | None ->
      let i = Gnt.Gnttab.interface_open () in
      cache := Some i;
      i

type mapping = Gnttab.Local_mapping.t with sexp_of

let buf_of_mapping x = Gnttab.Local_mapping.to_buf x

type grants = (int * grant) list with sexp_of

let mapv ~grants ~rw =
  let grants' = List.map (fun (domid, ref) -> { Gnttab.domid; ref = Int32.to_int ref }) grants in
  match Gnttab.mapv (open_gnttab ()) grants' rw with
  | None ->
    fail (Failure (Printf.sprintf "mapv grants=%s rw=%b" (Sexplib.Sexp.to_string_hum (sexp_of_grants grants)) rw))
  | Some x ->
    return x

let map ~domid ~grant ~rw =
  let grants = [ domid, grant ] in
  mapv ~grants ~rw

let unmap m = Gnttab.unmap_exn (open_gnttab ()) m

let description = "Memory pages will be shared using Xen grant tables."
