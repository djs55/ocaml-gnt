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

open Sexplib.Std

open Lwt

type 'a io = 'a Lwt.t

type grant = int32 with sexp

let grant_of_int32 x = x
let int32_of_grant x = x

type page = Cstruct.t
let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

type share = {
  grants: grant list;
  mapping: page;
} with sexp_of

let grants_of_share x = x.grants
let buf_of_share x = x.mapping

let get =
  let g = ref Int32.zero in
  fun () ->
    g := Int32.succ !g;
    Int32.pred !g

let rec get_n n =
  if n = 0 then [] else get () :: (get_n (n-1))

let individual_pages = Hashtbl.create 16
let big_mapping = Hashtbl.create 16

let rec to_pages remaining =
  if Cstruct.len remaining <= 4096
  then [ remaining ]
  else Cstruct.sub remaining 0 4096 :: (to_pages (Cstruct.shift remaining 4096))

let share ~domid ~npages ~rw ?(contents=`Zero) () =
  let grants = get_n npages in
  let mapping = match contents with
  | `Zero -> Cstruct.create (npages * 4096)
  | `Buffer b -> b in
  let share = { grants; mapping } in
  let pages = to_pages mapping in
  List.iter (fun (grant, page) -> Hashtbl.replace individual_pages grant page) (List.combine grants pages);
  Hashtbl.replace big_mapping (List.hd grants) mapping;
  return share

let remove tbl key =
  if Hashtbl.mem tbl key
  then Hashtbl.remove tbl key
  else begin
    Printf.fprintf stderr "Attempt to remove non-existing mapping\n%!";
    failwith "Attempt to remove non-existing mapping"
  end

let unshare share =
  List.iter (fun grant -> remove individual_pages grant) share.grants;
  remove big_mapping (List.hd share.grants);
  return ()

type mapping = {
  mapping: page;
  grants: (int * int32) list;
} with sexp_of

let buf_of_mapping x = x.mapping

let currently_mapped = Hashtbl.create 16

let map ~domid ~grant ~rw:_ =
  let mapping = Hashtbl.find individual_pages grant in
  if Hashtbl.mem currently_mapped grant then begin
    Printf.fprintf stderr "map: grant %ld is already mapped\n%!" grant;
    failwith (Printf.sprintf "map: grant %ld is already mapped" grant);
  end;
  Hashtbl.replace currently_mapped grant ();
  return { mapping; grants = [ domid, grant ] }

let mapv ~grants ~rw:_ =
  if grants = [] then begin
    Printf.fprintf stderr "mapv called with empty grant list\n%!";
    failwith "mapv: empty list"
  end;
  let first = snd (List.hd grants) in
  let mapping = Hashtbl.find big_mapping first in
  if Hashtbl.mem currently_mapped first then begin
    Printf.fprintf stderr "mapv: grant %ld is already mapped\n%!" first;
    failwith (Printf.sprintf "mapv: grant %ld is already mapped" first);
  end;
  Hashtbl.replace currently_mapped first ();
  return { mapping; grants }

let unmap { mapping; grants } =
  let first = snd (List.hd grants) in
  if Hashtbl.mem currently_mapped first
  then Hashtbl.remove currently_mapped first
  else begin
    Printf.fprintf stderr "unmap called with already-unmapped grant\n%!";
    failwith "unmap: already unmapped"
  end

let assert_cleaned_up () =
  if Hashtbl.length currently_mapped <> 0 then begin
    Printf.fprintf stderr "Some grants are still mapped in\n%!";
    failwith "some grants are still mapped in"
  end;
  if Hashtbl.length big_mapping <> 0 then begin
    Printf.fprintf stderr "Some grants are still active\n%!";
    failwith "some grants are still active"
  end

let description = "Memory pages will be shared by reference in the OCaml heap."
