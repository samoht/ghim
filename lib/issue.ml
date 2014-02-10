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

let bold fmt = Printf.sprintf ("\027[01m"^^fmt^^"\027[m")
let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

open Lwt
open Printf
open Github_t

type t = {
  user: string;
  repo: string;
  issue: Github_t.issue;
  token: Github.Token.t;
}

let string_of_state = function
  | `Open   -> "open"
  | `Closed -> "closed"

let list ~token ~user ~repo ~all =
  let open Github.Monad in

  run (

    let rec read milestone issues page state =
      printf "\rReading %s issues of %s/%s: page %d.%!"
        (string_of_state state) user repo page;

      Github.Issue.for_repo ~page ~token ~user ~repo ~milestone ~state ()
      >>= fun l ->
      if List.length l = 0 then (
        print_newline ();
        return issues
      ) else
        read milestone (l @ issues) (page+1) state in

    begin

      if all then (
        read `None [] 1 `Closed >>= fun none ->
        read `Any  [] 1 `Closed >>= fun all  ->
        return (none @ all)
      ) else
        return []

    end >>= fun closed ->

    read `None [] 1 `Open >>= fun none ->
    read `Any  [] 1 `Open >>= fun all  ->

    let issues = List.sort
        (fun i1 i2 -> compare i2.issue_number i1.issue_number)
        (none @ all @ closed) in

    let issues = List.map (fun issue ->
        { repo; user; issue; token }
      ) issues in

    return issues

  )

let pretty issues =
  printf "%s open issues found.\n%!" (bold "%d" (List.length issues));
  List.iter (fun t ->
      printf "%s %-13s  %s\n%!"
        (bold "%s/%s" t.user t.repo)
        (blue "#%d" t.issue.issue_number)
        t.issue.issue_title
    ) issues;
  return_unit

let (/) = Filename.concat

let comments { token; user; repo; issue = { issue_number } } =
  Github.Monad.run
    (Github.Issue.comments ~token ~user ~repo ~issue_number ())

let mkdir dirname =
  let rec aux dir =
    if not (Sys.file_exists dir) then (
      aux (Filename.dirname dir);
      Unix.mkdir dir 0o755
    ) in
  aux dirname

let expand issues =
  printf "%s open issues found, cloning.\n%!" (bold "%d" (List.length issues));
  Lwt_list.iter_s (fun t ->
      comments t >>= fun comments ->
      let files = [
        "title", t.issue.issue_title;
        "body" , t.issue.issue_body;
        "state", string_of_state t.issue.issue_state;
      ] @ List.map (fun c ->
          "comments" / string_of_int c.issue_comment_id, c.issue_comment_body
        ) comments
      in
      let prefix = t.user / t.repo / string_of_int t.issue.issue_number in
      List.iter (fun (file, contents) ->
          let file = prefix / file in
          mkdir (Filename.dirname file);
          let oc = open_out file in
          output_string oc contents;
          output_char oc '\n';
          flush oc;
          close_out oc
        ) files;
      return_unit;
    ) issues
