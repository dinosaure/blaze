Tests on make
  $ blaze.make --date 2009-07-12T12:00:00Z <<EOF
  > Hello World!
  > EOF
  Date: Sun, 12 Jul 2009 12:00:00 +0000
  Content-Transfer-Encoding: 7bit
  Content-Type: text/plain; charset=utf-8
  
  Hello World!
  $ blaze.make --date 2009-07-12T12:00:00Z --encoding quoted-printable <<EOF
  > Волим слани краставац.
  > EOF
  Date: Sun, 12 Jul 2009 12:00:00 +0000
  Content-Transfer-Encoding: quoted-printable
  Content-Type: text/plain; charset=utf-8
  
  =D0=92=D0=BE=D0=BB=D0=B8=D0=BC=20=D1=81=D0=BB=D0=B0=D0=BD=D0=B8=20=D0=BA=D1=
  =80=D0=B0=D1=81=D1=82=D0=B0=D0=B2=D0=B0=D1=86.
  $ blaze.make --date=none --from romain@blaze.org --to foo@bar.org,romain@gmail.com <<EOF
  > EOF
  To: foo@bar.org, romain@gmail.com
  Sender: romain@blaze.org
  From: romain@blaze.org
  Content-Transfer-Encoding: 7bit
  Content-Type: text/plain; charset=utf-8
  
  $ blaze.make --date none -f "Subject: Hello World!" <<EOF
  > EOF
  Content-Transfer-Encoding: 7bit
  Content-Type: text/plain; charset=utf-8
  Subject: Hello World!
  
  $ cat >body <<EOF
  > Hello World!
  > EOF
  $ blaze.make --date none body
  Content-Transfer-Encoding: 7bit
  Content-Type: text/plain; charset=utf-8
  
  Hello World!
  $ blaze.make --date none body -o email
  $ cat email
  Content-Transfer-Encoding: 7bit
  Content-Type: text/plain; charset=utf-8
  
  Hello World!
