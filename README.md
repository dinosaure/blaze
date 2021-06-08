# Blaze, some tools to manipulate your emails

`blaze` aggregates many tools to manipulate emails. The goal is to iterate
between high-level usages of emails and libraries such as [`mrmime`][mrmime],
[`dkim`][dkim] or [`spf`][spf]. It's an experimental repository!

## A simple example of `blaze`

As we said, `blaze` has many tools which want to be used into an UNIX context.
It permits to analyze, extract or stamp emails. This is an example of how to
use `blaze`:
```sh
$ git clone https://github.com/dinosaure/blaze
$ cd blaze
$ opam pin add -y .
# Assume that ~/maildir/ is a Maildir directory which contains your emails
$ export MSG=$(blaze.mdir new -D ~/maildir/ | tail -n1)
# We have the Maildir of the last message
$ blaze.mdir get -D ~/maildir/ --new $MSG > new.eml
# We can verify DKIM signature
$ blaze.dkim verify new.eml
[OK]: blaze.org
# Or SPF results
$ blaze.spf analyze new.eml
romain@blaze.org from 192.168.0.0: pass (expected: pass)
# And extract some informations from it
$ blaze.addr --without-name new.eml
romain@blaze.org
```

[mrmime]: https://github.com/mirage/mrmime
[dkim]: https://github.com/dinosaure/ocaml-dkim
[spf]: https://github.com/dinosaure/ocaml-spf
