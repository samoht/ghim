## Ghim

A command-line tool to manage Github Issues.

Currently it doesn't do much (you can just list the repository issues),
but the plan is to map them to some git-based local filesystem than you
can then grep, modify, pull/push etc.

### Build

You will need [ocaml-github](https://github.com/avsm/ocaml-github),
[dolog](https://github.com/UnixJunkie/dolog) and
 [cmdliner](https://github.com/dbuenzli/cmdliner):

```shell
opam install github dolog cmdliner
```

To compile the project, you can then just run:
```shell
make
make install
```

### Examples

#### Listing the remote issues

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

#### Mapping the remote issues to your local filesystem

```shell
~/git/ghim$ ghim clone mirage
48 issues found, cloning.
$ grep network -r mirage/mirage
mirage/mirage/128/body:- [ ] unix-networking -- @avsm
mirage/mirage/186/body:# networking
mirage/mirage/186/body:* I use PF available on the host to do the network plumbing required for the
mirage/mirage/81/body:(there may be other issues but) the problems i saw were due to the openflow module's use of Net facilities which don't yet have dummies in the node version of the networking, e.g. OS.Netif, Net.Manager.interface.
```