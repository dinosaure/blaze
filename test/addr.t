Tests on addresses
  $ echo "From: romain.calascibetta@blaze.com\n" | blaze.addr
  romain.calascibetta@blaze.com
  $ cat >simple <<EOF
  > From: a@foo.com
  > Sender: b@bar.org
  > 
  > EOF
  $ blaze.addr simple
  a@foo.com
  b@bar.org
  $ cat >simple <<EOF
  > BLAZE-From: romain@blaze.org
  > From: romain@foo.com
  > 
  > EOF
  $ blaze.addr -f BLAZE-From simple
  romain@blaze.org
  romain@foo.com
  $ blaze.addr -f BLAZE-From:BLAZE-To <<EOF
  > BLAZE-From: romain@blaze.org
  > BLAZE-To: anil@blaze.org
  > From: romain@foo.com
  > To: anil@bar.org
  > 
  > EOF
  romain@blaze.org
  anil@blaze.org
  romain@foo.com
  anil@bar.org
  $ echo "From: θσερ@εχαμπλε.ψομ\n" | blaze.addr
  θσερ@εχαμπλε.ψομ
  $ echo "From: romain@foo.com\n" | blaze.addr -
  romain@foo.com
  $ blaze.addr 001.mail
  Thomas Gazagnaire <notifications@github.com>
  mirage/irmin <reply+0004e976450d3015e434a818b602473d45afa079962ab62292cf00000001143b44b392a169ce0b37f3a2@reply.github.com>
  mirage/irmin <irmin@noreply.github.com>
  $ blaze.addr -d 002.mail
  Yann Régis Gianas <notifications@github.com>
  mirage/decompress <reply+0004e976d9ee97bc1d2d238f13f646219cc36441871e86af92cf000000011820dbf092a169ce1722a3ee@reply.github.com>
  mirage/decompress <decompress@noreply.github.com>
  Mention <mention@noreply.github.com>
  Calascibetta Romain <romain.calascibetta@gmail.com>
  $ blaze.addr -d 003.mail
  Thomas Braibant <tbraibant@janestreet.com>
  Romain Calascibetta <romain.calascibetta@gmail.com>
  Gabriel Scherer <gabriel.scherer@inria.fr>
  Louis Roché <louis@cryptosense.com>
  Roberto Di Cosmo <roberto@dicosmo.org>
  $ blaze.addr --without-name 002.mail
  notifications@github.com
  reply+0004e976d9ee97bc1d2d238f13f646219cc36441871e86af92cf000000011820dbf092a169ce1722a3ee@reply.github.com
  decompress@noreply.github.com
  mention@noreply.github.com
  romain.calascibetta@gmail.com
