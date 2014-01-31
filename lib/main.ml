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
open Cmdliner

(* Global options *)
type global = {
  level: Log.log_level option;
}

let app_global g =
  Log.color_on ();
  match g.level with
  | None   -> ()
  | Some d -> Log.set_log_level d

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ghit/issues.";
]

let global =
  let debug =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be very verbose." ["debug"] in
    Arg.(value & flag & doc) in
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let level debug verbose =
    match debug, verbose with
    | true, _    -> { level = Some Log.DEBUG }
    | _   , true -> { level = Some Log.INFO }
    | _          -> { level = None } in
  Term.(pure level $ debug $ verbose)

let user =
  let doc = Arg.info ~docv:"USER"
      ~doc:"The name of the user who host the repository." [] in
  Arg.(required & pos 0 (some string) None & doc)

let repository =
  let doc = Arg.info ~docv:"REPOSITORY"
      ~doc:"The repository name." [] in
  Arg.(required & pos 1 (some string) None & doc)


let jar =
  let doc = Arg.info ~docv:"JAR"
      ~doc:"Use github-jar to authentificate." ["j";"jar"] in
  Arg.(value & opt (some string) None & doc)

let token =
  let doc = Arg.info ~docv:"TOKEN"
      ~doc:"Use a token to authentificate." ["t";"token"] in
  Arg.(value & opt (some string) None & doc)

let password =
  let doc = Arg.info ~docv:"PASSWORD"
      ~doc:"Use a login:password:id combination to authentificate \
            (not recommended)."
      ["p";"password"] in
  Arg.(value & opt (some (list ~sep:':' string)) None & doc)

let run t =
  Lwt_unix.run (
    catch
      (fun () -> t)
      (function e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun global -> app_global global; fn) $ global)

type sub = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let create_command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man


(* LIST *)
let list = {
  name = "list";
  doc  = "List of the open issues in a given repository.";
  man  = [];
  term =

    let rec auth jar token pass =
      match jar, token, pass with
      | Some j, None, None ->
        begin Github_cookie_jar.init () >>= fun t ->
          Github_cookie_jar.get t ~name:j >>= function
          | None       -> fail (Failure (j ^ ": unknown jar."))
          | Some oauth -> return (Github.Token.of_auth oauth)
        end
      | None, Some t, None ->
        return (Github.Token.of_string t)
      | None, None, Some [u; p; i] ->
        Github.Monad.run (Github.Token.get ~user:u ~pass:p ~id:(int_of_string i) ()) >>= fun a ->
        return (Github.Token.of_auth a)
      | None, None, Some _ ->
        failwith "Invalid credentials. You should provide: $(b,-p login:password:id)"
      | _ -> auth (Some "local") None None in

    let list jar token pass user repo =
      run begin
        auth jar token pass           >>= fun token ->
        Issue.all ~token ~user ~repo >>= fun issues ->
        Issue.pretty issues;
        return_unit
      end in
    Term.(mk list $ jar $ token $ password $ user $ repository)
}

let default =
  let doc = "The GitHub Issues Manager." in
  let man = [
    `S "DESCRIPTION";
    `P "A command-line tool to manage Github Issues locally.";
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] in
  let usage global =
    app_global global;
    Printf.printf
      "usage: ghim [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
      \n\
      The most commonly used irminsule commands are:\n\
      \    list        %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      list.doc in
  Term.(pure usage $ global),
  Term.info "irmin"
    ~version:"0.1"
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map create_command [
  list;
  ]

let () =
  match Cmdliner.Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _        -> ()
