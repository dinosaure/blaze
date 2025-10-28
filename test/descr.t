Tests on descr
  $ export BLAZE_UTF_8=false
  $ echo "Foo" > text
  $ blaze make <<EOF | blaze make wrap --boundary foobar --field=X-Blaze:Dezalb | blaze make put --field X-Blaze:Blazed --encoding quoted-printable text | blaze make wrap --boundary barfoo --related | blaze make put text > new.eml
  > Hello
  > EOF
  $ blaze descr new.eml
  .-- related
  |   |-- mixed
  |   |   |-- text/plain
  |   |   `-- text/plain
  |   `-- text/plain
