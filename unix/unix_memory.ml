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
open Sexplib.Std
open Lwt

(* A shared block of memory is represented by a file with path
   $XEN_ROOT/memory/<g0,g1,g2,gN> *)

let env_var = "XEN_ROOT"

let cache f =
  let c = ref None in
  fun () -> match !c with
  | Some x -> return x
  | None ->
    f ()
    >>= fun path ->
    c := Some path;
    return path

let get_xen_root = cache (fun () ->
  let rec loop counter =
    if counter > 100 then begin
      Printf.fprintf stderr "Failed to create a private $%s dir (I tried > 100 times!)\n%!" env_var;
      fail (Failure (Printf.sprintf "failed to create private $%s dir" env_var))
    end else begin
      let path = Filename.(concat temp_dir_name (Printf.sprintf "%s.%d.%d" (basename Sys.argv.(0)) (Unix.getpid ()) counter)) in
      Lwt.catch
        (fun () ->
          Lwt.catch
            (fun () -> Lwt_unix.access path [ Lwt_unix.X_OK ])
            (fun _ ->
              Lwt_unix.mkdir path 0o0700
              >>= fun () ->
              at_exit (fun () -> Unix.rmdir path);
              return ()
            )
          >>= fun () ->
          return path
        ) (fun _ -> loop (counter + 1))
      end in
  (* First look for a path in an environment variable *)
  Lwt.catch
    ( fun () ->
      (try return (Sys.getenv env_var) with e -> fail e)
      >>= fun root ->
      Lwt_unix.access root [ Lwt_unix.X_OK ]
      >>= fun () ->
      return root )
    (fun _ ->
      (* Fall back to creating a fresh path *)
      loop 0
      >>= fun path ->
      (* Print the environment variable needed for other apps to talk to us *)
      Printf.fprintf stderr "%s=%s; export %s;\n%!" env_var path env_var;
      return path)
)

let get_memory_dir = cache (fun () ->
  get_xen_root ()
  >>= fun root ->
  let dir = Filename.concat root "memory" in
  Lwt.catch
    (fun () -> Lwt_unix.access dir [ Lwt_unix.X_OK ])
    (fun _ ->
      Lwt_unix.mkdir dir 0o0700
      >>= fun () ->
      at_exit (fun () -> Unix.rmdir dir);
      return ()
    )
  >>= fun () ->
  return dir
)
type 'a io = 'a Lwt.t

type grant = int32 with sexp

let grant_of_int32 x = x
let int32_of_grant x = x

type page = Io_page.t
let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

type share = {
  grants: grant list;
  mapping: page;
} with sexp_of

let grants_of_share x = x.grants
let buf_of_share x = x.mapping
let filename_of_grants grants =
  get_memory_dir ()
  >>= fun dir ->
  let basename = String.concat "," (List.map Int32.to_string grants) in
  return (Filename.concat dir basename)

let get =
  let g = ref Int32.zero in
  fun () ->
    g := Int32.succ !g;
    Int32.pred !g

let rec get_n n =
  if n = 0 then [] else get () :: (get_n (n-1))

let share ~domid ~npages ~rw =
  let grants = get_n npages in
  let size = npages * 4096 in
  filename_of_grants grants
  >>= fun name ->
  Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_TRUNC; Lwt_unix.O_RDWR ] 0o0600
  >>= fun fd ->
  (*
  Lwt_unix.lseek fd (npages * 4096 - 1) Unix.SEEK_SET
  >>= fun _ ->
  Lwt_unix.write fd "\000" 0 1
  >>= fun n ->
  *)
  let unix_fd = Lwt_unix.unix_file_descr fd in
  let mapping = Lwt_bytes.map_file ~fd:unix_fd ~shared:true ~size () in
  Lwt_unix.close fd
  >>= fun () ->
  (*
  if n <> 1 then begin
    let msg = Printf.sprintf "Failed to create %s with %d pages" name npages in
    Printf.forintf stderr "%s\n%!" msg;
    fail (Failure msg)
  end else
  *)
  return { grants; mapping }

let unshare share =
  filename_of_grants share.grants
  >>= fun name ->
  Lwt_unix.unlink name

type mapping = {
  mapping: page;
  grants: (int * int32) list;
} with sexp_of

let buf_of_mapping x = x.mapping

let mapv ~grants ~rw =
  filename_of_grants (List.map snd grants)
  >>= fun name ->
  Lwt_unix.openfile name [ Lwt_unix.O_RDWR ] 0o0600
  >>= fun fd ->
  let unix_fd = Lwt_unix.unix_file_descr fd in
  let mapping = Lwt_bytes.map_file ~fd:unix_fd ~shared:true () in
  Lwt_unix.close fd
  >>= fun () ->
  return { mapping; grants }

let map ~domid ~grant ~rw =
  let grants = [ domid, grant ] in
  mapv ~grants ~rw

let unmap { mapping; grants } = ()

let assert_cleaned_up () = ()

let description = "Memory pages will be shared using mmap(2)."

