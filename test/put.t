Tests on put
  $ echo "J'adore le concombre salé." > text
  $ blaze.make --date none --encoding quoted-printable <<EOF | blaze.make wrap --boundary foobar | blaze.make put --encoding quoted-printable text
  > Волим слани краставац.
  > EOF
  Content-Type: multipart/mixed; boundary=foobar
  MIME-Version: 1.0
  
  --foobar
  Content-Type: text/plain; charset=utf-8
  Content-Transfer-Encoding: quoted-printable
  
  =D0=92=D0=BE=D0=BB=D0=B8=D0=BC=20=D1=81=D0=BB=D0=B0=D0=BD=D0=B8=20=D0=BA=D1=
  =80=D0=B0=D1=81=D1=82=D0=B0=D0=B2=D0=B0=D1=86.
  
  --foobar
  Content-Transfer-Encoding: quoted-printable
  Content-Type: text/plain; charset=utf-8
  
  J'adore=20le=20concombre=20sal=C3=A9.
  --foobar--
