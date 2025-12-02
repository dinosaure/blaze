Test about the PACK file
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
  $ blaze pack get pack.pack 264f851c7be86a4e4d9a93ec0a2c781027b9eda5 > 001.eml
  $ blaze pack get pack.pack ebd007451f3c780e28542e44b6e2773aaee02113 > 002.eml
  $ blaze pack get pack.pack 38e557e1906cc574b41440110cc1b64c402d360a > 003.eml
  $ blaze pack get pack.pack f7d6961360997bbcd2fb1053e9049a9dcb844496 > 004.eml
  $ diff 001.eml 01.eml
  $ diff 002.eml 02.eml
  $ diff 003.eml 03.eml
  $ diff 004.eml 04.eml
