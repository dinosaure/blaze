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

## How to craft & send an email?

`blaze` wants to be simple to send an email. First, you need to craft
one with some meta-information. Then, you can attach a document to your
email and then, pass it to `blaze.send` with your account credentials:
```sh
$ blaze.make <<EOF \
  | blaze.make wrap --mixed
  | blaze.make put --encoding base64 image.png
  | blaze.send --sender foo@bar --password ****** smtp.bar
> Hello World!
> EOF
```

As you can see, you can craft and send your email along the UNIX pipe.

`blaze` has received funding from the Next Generation Internet Initiative
(NGI) within the framework of the DAPSI Project.

[mrmime]: https://github.com/mirage/mrmime
[dkim]: https://github.com/dinosaure/ocaml-dkim
[spf]: https://github.com/dinosaure/ocaml-spf
