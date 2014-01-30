(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Printf

let iter ~fn ~token ~user ~repo =
  let open Github.Monad in
  let open Github_t in

  run (

    let rec read milestone issues page =
      Github.Issue.for_repo ~page ~token ~user ~repo ~milestone () >>= fun l ->
      if List.length l = 0 then
        return issues
      else
        read milestone (l @ issues) (page+1) in

    read `None [] 1 >>= fun none ->
    read `Any [] 1  >>= fun all  ->

    let issues = List.sort
        (fun i1 i2 -> compare i2.issue_number i1.issue_number)
        (none @ all) in

    fn issues

  )
