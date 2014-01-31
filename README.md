## Ghim

A command-line tool to manage Github Issues.

Currently it doesn't do much (you can just list the repository issues),
but the plan is to map them to some git-based local filesystem than you
can then grep, modify, pull/push etc.

### Build

You will need [ocaml-github](https://github.com/avsm/ocaml-github)
 and [cmdliner](https://github.com/dbuenzli/cmdliner).

You can then just run:
```shell
make
make install
```

### Examples

```shell
~/git/ghim$ ghim list mirage
48 issues found.
mirage/mirage #205    too little logging output when invoking `opam install`
mirage/mirage #204    clean is too aggressive
mirage/mirage #203    V2 improvements:
mirage/mirage #202    apparently odd set of dependencies triggering opam behaviour
mirage/mirage #201    Integrate @mor1 patch to fix the mode selection
mirage/mirage #200    V2: Need a write function in the console signature
mirage/mirage #199    Unify the mirage-types and mirage repositories
mirage/mirage #198    mirage-console is missing a Travis
mirage/mirage #193    Testing: packet generation using Ostinato
[...]
```