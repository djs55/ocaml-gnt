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
open Memory
open Gnt
open Sexplib.Std
open Lwt

type 'a io = 'a Lwt.t

type grant = int32 with sexp

let grant_of_int32 x = x
let int32_of_grant x = x

let open_gntshr =
  let cache = ref None in
  fun () -> match !cache with
    | Some x -> x
    | None ->
      let i = Gnt.Gntshr.interface_open () in
      cache := Some i;
      i

type page = Cstruct.t
let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

type share =
| Share of Gntshr.share
| Refs of page * (Gnt.gntref list)
with sexp_of

let grants_of_share = function
| Share share -> List.map Int32.of_int share.Gntshr.refs
| Refs (_, refs) -> List.map Int32.of_int refs

let buf_of_share = function
| Share share -> Io_page.to_cstruct share.Gntshr.mapping
| Refs (buffer, _) -> buffer

let rec to_pages remaining =
  if Cstruct.len remaining <= 4096
  then [ remaining ]
  else Cstruct.sub remaining 0 4096 :: (to_pages (Cstruct.shift remaining 4096))

let share ~domid ~npages ~rw ?(contents=`Zero) () =
  match contents with
  | `Zero ->
    begin match Gntshr.share_pages (open_gntshr ()) domid npages rw with
    | None ->
      fail (Failure (Printf.sprintf "share_pages domid=%d npages=%d rw=%b" domid npages rw))
    | Some s ->
      return (Share s)
    end
  | `Buffer b ->
    Gntshr.get_n npages
    >>= fun grefs ->
    let pages = match npages with
    | 1 -> [ b ]
    | _ ->
      (* XXX: this is inefficient because mirage-platform stub_gntshr_grant_access
      calls [base_page_of] which queries the [data] pointer of the bigarray *)
      List.map Io_page.to_cstruct Io_page.(to_pages (Cstruct.to_bigarray b)) in
    (* pages are now Cstruct wrappers around Io_pages which are safe to share *)
    List.iter
      (fun (gref, page) ->
        (* This grants access to the *base* data pointer of the page *)
        (* XXX: another place where we peek inside the cstruct *)
        if page.Cstruct.off <> 0 then begin
          Printf.printf "Memory.share: supplied buffer not page-aligned\n%!";
          (* This is a fatal error in the application *)
          failwith "Memory.share: supplied buffer not page-aligned";
        end;
        Gnt.Gntshr.grant_access ~domid ~writable:rw gref page.Cstruct.buffer
      ) (List.combine grefs pages);
    return (Refs (b, grefs))


let unshare = function
| Share share ->
  Gntshr.munmap_exn (open_gntshr ()) share;
  return ()
| Refs (_, grefs) ->
  List.iter
    (fun gref ->
      Gnt.Gntshr.end_access gref;
      Gnt.Gntshr.put gref;
    ) grefs;
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

let buf_of_mapping x = Io_page.to_cstruct (Gnttab.Local_mapping.to_buf x)

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
