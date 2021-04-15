Test on headers
  $ echo "From: romain.calascibetta@blaze.com,\n root@foo.org\nTo: root@bar.org\n" | blaze.hdr
  From: romain.calascibetta@blaze.com,
        root@foo.org
  To: root@bar.org
  $ blaze.hdr <<EOF
  > Date: Thu, 30 Mar 2017 15:00:00 +0200
  > 
  > EOF
  Date: 2017-03-30 15:00:00 +02:00
  $ blaze.hdr <<EOF
  > Subject: Hello World!
  > 
  > EOF
  Subject: Hello World!
  $ blaze.hdr <<EOF | cat -e
  > Subject:
  > 
  > EOF
  Subject:$
  $ echo "Content-Type: text/example\n" | blaze.hdr
  Content-Type: text/example
  $ echo "Content-Type: text/example; charset=utf-8\n" | blaze.hdr
  Content-Type: text/example; charset=utf-8
  $ echo "A: foo\nB: bar\n" | blaze.hdr
  A: foo
  B: bar
  $ echo "A: foo\nB: bar\n" | blaze.hdr -h A
  A: foo
  $ echo "A: foo\nB: bar\n" | blaze.hdr -h B
  B: bar
  $ echo "A: foo\nA: bar\n" | blaze.hdr -h A
  A: foo
  $ echo "A: foo\nA: bar\n" | blaze.hdr -h A:A
  A: foo
  A: bar
  $ blaze.hdr -h From:To <<EOF
  > From: romain@blaze.org
  > Subject: Hello World!
  > To: foo@blaze.org
  > To: bar@blaze.org
  > 
  > EOF
  From: romain@blaze.org
  To: foo@blaze.org
  $ blaze.hdr -h Content-Type -p charset <<EOF
  > Content-Type: text/example; charset=utf-8
  > 
  > EOF
  utf-8
  $ blaze.hdr -h Content-Type:Google-Content-type -p charset <<EOF
  > Content-Type: text/ascii; charset=utf-8
  > GOOGLE-Content-Type: text/utf-8; charset=utf-8
  > 
  > EOF
  utf-8
  utf-8
  $ echo "From: =?US-ASCII?Q?Keith_Moore?= <keith.moore@blaze.org>\n" | blaze.hdr
  From: =?US-ASCII?Q?Keith_Moore?= <keith.moore@blaze.org>
  $ echo "From: =?US-ASCII?Q?Keith_Moore?= <keith.moore@blaze.org>\n" | blaze.hdr -d
  From: Keith Moore <keith.moore@blaze.org>
  $ echo "From: romain@blaze.org\n" | blaze.hdr -H
  >	From: romain@blaze.org
  $ cat >email <<EOF
  > From: romain.calascibetta@blaze.org
  > 
  > EOF
  $ blaze.hdr -H email
  email	From: romain.calascibetta@blaze.org
  $ blaze.hdr -h Subject:Date 001.mail
  Date: 2016-11-09 12:10:27 -08:00
  Subject: Re: [mirage/irmin] use a numerical version in META (_NUM strips leading v) (#378)
  $ blaze.hdr -h Date:Subject 002.mail
  Date: 2018-12-06 04:45:36 -08:00
  Subject: Re: [mirage/decompress] Outdated documentation (#65)
  $ blaze.hdr -d -h From:Date:Subject:To 003.mail
  From: Thomas Braibant <tbraibant@janestreet.com>
  Date: 2015-10-29 11:38:12 -04:00
  Subject: Salle Algorithme, 10 novembre
  To: Roberto Di Cosmo <roberto@dicosmo.org>,
      Louis Roché <louis@cryptosense.com>,
      Gabriel Scherer <gabriel.scherer@inria.fr>,
      Romain Calascibetta <romain.calascibetta@gmail.com>
