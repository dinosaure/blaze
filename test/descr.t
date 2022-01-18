Tests on descr
  $ export BLAZE_UTF_8=false
  $ echo "Foo" > text
  $ blaze.make <<EOF | blaze.make wrap --boundary foobar --field=X-Blaze:Dezalb | blaze.make put --field X-Blaze:Blazed --encoding quoted-printable text | blaze.make wrap --boundary barfoo --related | blaze.make put text > new.eml
  > Hello
  > EOF
  $ blaze.descr --fields=content-type,content-transfer-encoding,x-blaze new.eml
  Content-Type: multipart/iana:related boundary=barfoo
  |- Content-Type: multipart/iana:mixed boundary=foobar
  `- Content-Transfer-Encoding: 7bit
   |- Content-Type: text/iana:plain charset=utf-8
   |  Content-Transfer-Encoding: 7bit
   `- X-Blaze: Dezalb
  
   |- Content-Transfer-Encoding: quoted-printable
   |  Content-Type: text/iana:plain charset=utf-8
   `- X-Blaze: Blazed
  
  |- Content-Transfer-Encoding: 7bit
  `- Content-Type: text/iana:plain charset=utf-8
  
