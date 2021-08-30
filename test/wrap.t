Tests on wrap
  $ blaze.make --date 2009-07-12T12:00:00Z <<EOF | blaze.make wrap --seed mDKn75q9VvfqX95pjM08k6zhsrEnsYR714L3MF9s9GA= -
  > Hello World!
  > EOF
  MIME-Version: 1.0
  Date: Sun, 12 Jul 2009 12:00:00 +0000
  Content-Type: multipart/mixed; boundary="wZRXosmqA5w="
  
  --wZRXosmqA5w=
  Content-Type: text/plain; charset=utf-8
  Content-Transfer-Encoding: 7bit
  
  Hello World!
  
  --wZRXosmqA5w=--
  $ blaze.make --date 2009-07-12T12:00:00Z --encoding quoted-printable <<EOF | blaze.make wrap --boundary foobar
  > Волим слани краставац.
  > EOF
  MIME-Version: 1.0
  Date: Sun, 12 Jul 2009 12:00:00 +0000
  Content-Type: multipart/mixed; boundary=foobar
  
  --foobar
  Content-Type: text/plain; charset=utf-8
  Content-Transfer-Encoding: quoted-printable
  
  =D0=92=D0=BE=D0=BB=D0=B8=D0=BC=20=D1=81=D0=BB=D0=B0=D0=BD=D0=B8=20=D0=BA=D1=
  =80=D0=B0=D1=81=D1=82=D0=B0=D0=B2=D0=B0=D1=86.=0D=0A
  
  --foobar--
