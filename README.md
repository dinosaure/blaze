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
$ export MSG=$(blaze mdir new -D ~/maildir/ | tail -n1)
# We have the Maildir of the last message
$ blaze mdir get -D ~/maildir/ --new $MSG > new.eml
# We can verify DKIM signature
$ blaze dkim verify new.eml
[OK]: blaze.org
# Or SPF results
$ blaze spf analyze new.eml
romain@blaze.org from 192.168.0.0: pass (expected: pass)
# And extract some informations from it
$ blaze addr --without-name new.eml
romain@blaze.org
```

## How to craft & send an email?

`blaze` wants to be simple to send an email. First, you need to craft
one with some meta-information. Then, you can attach a document to your
email and then, pass it to `blaze send` with your account credentials. You
may be need another tool, [conan][conan], to be able to recognize the MIME
type of the attachment.
```sh
$ blaze make <<EOF \
  | blaze make wrap --mixed
  | blaze make put --type $(conan.file --mime image.png) \
    --encoding base64 image.png
  | blaze send --sender foo@bar - smtp.bar
> Hello World!
> EOF
```

As you can see, you can craft and send your email along the UNIX pipe.

## How to receive an email?

`blaze` provides a little server which is able to receive emails. You can
receive one or some depending on arguments given to `blaze srv`. By default, it
wants to listen on `*:25`. It's an implementation of a simple SMTP server and
it waiting an email to save it then into your file-system:
```
$ sudo blaze srv -o new.eml &
[1] PID
$ blaze make <<EOF \
  | blaze send --sender foo@bar -r admin@localhost
> Hello World!
> EOF
$ cat new.eml
Date: Thu, 6 Jan 2022 16:49:55 +0100
Content-Transfer-Encoding: 7bit
Content-Type: text/plain; charset=utf-8

Hello World!
```

To receive multiple emails, you can launch `blaze srv` like this:
```sh
$ sudo socat TCP-LISTEN:25,fork TCP:localhost:4242 &
$ mkdir mailbox
$ blaze srv -o mailbox/ --format "blaze-%s.eml" localhost:4242 &
$ blaze make <<EOF | blaze send --sender din@osau.re -r foo@bar - localhost:25
> Hello World!
> EOF
```

## Submit or send an email?

SMTP is a bi-diretional protocol where a SMTP server can be a client for
another SMTP server. When you want to send an email, you have 2 possibilities:
1) send an email to a recipient directly under my identity concretized _via_
   my computer (my _hostname_, etc.)
2) send an email to a recipient **through** a _certain_ identity provided by
   a SMTP server (like `gmail.com`)

For the first case, you want to use `blaze send` which sends an email directly
to the given SMTP server (on `*:25`) or the SMTP server of the first recipient.
For instance, this command send an email to `foo:25`:
```sh
$ blaze send --sender romain@blaze.org -r bar@foo
```

However, you probably want some security mechanisms such as DKIM or SPF offered
by a _certain_ service like `gmail.com`. In that case, you want to **submit**
an email to this service which will re-send your email under **its**
authority with its security mechanisms:
```sh
$ blaze submit --sender romain@blaze.org --password ****** -r bar@foo
```

The second case ensure that the communication between you and the SMTP service
is encrypted (_via_ `STARTTLS` or over `TLS`). Otherwise, it does not try to
send an email. Then, the submission server **is not** `foo:25` like before but
`blaze.org:{465,587}`. Usually, this service requires a password.

In both case, `bar@foo` will receive an email but:
- in the first case, it will receive the email as is
- in the second case, your _serviteur_ (`blaze.org`) probably put some
  _metadata_ to let `bar@foo` to _verify_ the given email

## How to fetch emails?

blaze also lets you download emails and store them in a folder. There are
additional tools for storing these emails in ‘maildir’, ‘mbox’ or our special
‘pack’ format.

You can currently download emails from your GMail account for the last 30 days
in this way:
```sh
$ mkdir mailbox
$ blaze fetch pop3://pop.gmail.com:995 --username recent:<username>@gmail.com \
  --password ****** -f "mailbox/%s.eml" > emails.txt
```

You can then generate an archive of these emails in our PACK format:
```sh
$ blaze pack make -o pack.pack emails.txt
$ blaze pack index pack.pack
```

## How to store emails?

As mentioned above, blaze prefers to store emails in a special format, the PACK
format. It is possible to switch from a format such as mbox to the PACK format
if you wish, so that you use less disk space to store your emails.
```sh
$ blaze mbox archive.mbox -o pack.pack
$ blaze pack index pack.pack
```

## Funding

This project received funding through [NGI Zero Core](https://nlnet.nl/core), a
fund established by [NLnet](https://nlnet.nl) with financial support from the
European Commission's [Next Generation Internet](https://ngi.eu) program. Learn
more at the [NLnet project page](https://nlnet.nl/project/PTT).

[mrmime]: https://github.com/mirage/mrmime
[dkim]: https://github.com/dinosaure/ocaml-dkim
[spf]: https://github.com/dinosaure/uspf
[conan]: https://github.com/mirage/conan
