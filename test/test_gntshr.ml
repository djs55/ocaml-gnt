(* Copyright (C) 2014 Citrix Inc
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
let (|>) a b = b a
open OUnit
open Gnt

let get_my_domid () =
  let module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client) in
  let open Lwt in
  Lwt_main.run (
    Xs.make ()
    >>= fun c ->
    Xs.(immediate c (fun h -> read h "domid")) >>= fun server_domid ->
    return (int_of_string server_domid)
  )

let check_read_write () =
  let shr_h = Gntshr.interface_open () in
  let dev_h = Gnttab.interface_open () in
  let domid = get_my_domid () in
  let share = Gntshr.share_pages_exn shr_h domid 1 true in
  let io_page_shr_side = Gntshr.(share.mapping) |> Cstruct.of_bigarray in
  List.iter (fun r -> Printf.printf "Shared a page with gntref = %d\n%!" r) Gntshr.(share.refs);
  Printf.printf "Shared page(s) OK. Now trying to map.\n%!";
  let local_mapping = Gnttab.map_exn dev_h Gnttab.({domid; ref=List.hd Gntshr.(share.refs)}) true in
  let io_page_map_side = Gnttab.Local_mapping.(to_buf local_mapping) |> Cstruct.of_bigarray in
  Printf.printf "Mapping OK. Now writing randow stuff in one side and check we have the same thing on the other side.\n%!";
  let random_string = String.create 4096 in
  let zero_string = String.make 4096 '\000' in
  let zero_string2 = String.make 4096 '\000' in
  Cstruct.blit_from_string random_string 0 io_page_shr_side 0 4096;
  Cstruct.blit_to_string io_page_shr_side 0 zero_string 0 4096;
  assert (zero_string = random_string);
  Printf.printf "I blitted random 4096 chars on the page, and read back the same, all OK!\n%!";
  Cstruct.blit_to_string io_page_map_side 0 zero_string2 0 4096;
  assert (zero_string2 = random_string);
  Printf.printf "I read the same as well from the map side...\n%!";
  Printf.printf "Now unmapping and unsharing everything.\n%!";
  Gnttab.unmap_exn dev_h local_mapping;
  Gntshr.munmap_exn shr_h share;
  Printf.printf "Now trying to share and map 10 pages as a vector.\n%!";
  let share = Gntshr.share_pages_exn shr_h domid 10 true in
  let io_page_shr_side = Gntshr.(share.mapping) |> Cstruct.of_bigarray in
  let refs = Gntshr.(share.refs) in
  let grants = List.map (fun ref -> Gnttab.({domid; ref})) refs in
  let local_mapping = Gnttab.mapv_exn dev_h grants true in
  let io_page_map_side = Gnttab.Local_mapping.(to_buf local_mapping) |> Cstruct.of_bigarray in
  let random_string = String.create (4096*10) in
  let zero_string = String.make (4096*10) '\000' in
  let zero_string2 = String.make (4096*10) '\000' in
  Cstruct.blit_from_string random_string 0 io_page_shr_side 0 (4096*10);
  Cstruct.blit_to_string io_page_shr_side 0 zero_string 0 (4096*10);
  assert (zero_string = random_string);
  Printf.printf "I blitted random 4096*10 chars on the page, and read back the same, all OK!\n%!";
  Cstruct.blit_to_string io_page_map_side 0 zero_string2 0 (4096*10);
  assert (zero_string2 = random_string);
  Printf.printf "I read the same as well from the map side...\n%!";
  Printf.printf "Success! Now unmapping and unsharing everything!\n%!";
  Gnttab.unmap_exn dev_h local_mapping;
  Gntshr.munmap_exn shr_h share;
  Gntshr.interface_close shr_h;
  Gnttab.interface_close dev_h

let leak_free n () =
  let shr_h = Gntshr.interface_open () in
  let dev_h = Gnttab.interface_open () in
  for i = 0 to 10000 do
    let share = Gntshr.share_pages_exn shr_h 0 n true in
    Gntshr.munmap_exn shr_h share
  done;
  Gntshr.interface_close shr_h;
  Gnttab.interface_close dev_h

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test gntshr/gnttab code";

  let suite = "gnt" >::: [
    "leak_free 1 page" >:: leak_free 1;
    "leak_free 2 pages" >:: leak_free 2;
    "check_read_write" >:: check_read_write;
  ] in
  run_test_tt ~verbose:!verbose suite
