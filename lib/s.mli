(*
 * Copyright (c) 2013,2014 Citrix Systems Inc
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

module type MEMORY = sig
  type 'a io = 'a Lwt.t

  type grant with sexp

  val grant_of_int32: int32 -> grant
  val int32_of_grant: grant -> int32

  type share with sexp_of

  val grants_of_share: share -> grant list
  val buf_of_share: share -> Io_page.t

  val share: domid:int -> npages:int -> rw:bool -> share io

  val unshare: share -> unit io

  type mapping with sexp_of

  val buf_of_mapping: mapping -> Io_page.t

  val map: domid:int -> grant:grant -> rw:bool -> mapping io

  val mapv: grants:(int * grant) list -> rw:bool -> mapping io

  val unmap: mapping -> unit

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)
end
