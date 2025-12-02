Test about search engine
  $ blaze crlf 001.mail > 01.eml
  $ blaze crlf 002.mail > 02.eml
  $ blaze crlf 003.mail > 03.eml
  $ blaze crlf 004.mail > 04.eml
  $ blaze pack make -o pack.pack <<EOF
  > 01.eml
  > 02.eml
  > 03.eml
  > 04.eml
  > EOF
  $ blaze pack index pack.pack
  $ blaze okapi pack.idx "decompress" | head -n1 | cut -d':' -f1
  ebd007451f3c780e28542e44b6e2773aaee02113
