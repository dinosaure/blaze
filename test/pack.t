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
  $ blaze pack get pack.pack d722e62a30d2362da3078be4b57091b8335cda75 > 001.eml
  $ blaze pack get pack.pack aa970fb1760f622e2e966779767f85f82d9714e8 > 002.eml
  $ blaze pack get pack.pack 299e18f2598abe172368ed1be8cbcd29cbd9c847 > 003.eml
  $ blaze pack get pack.pack 1e89ec6e821ff96ad4dd4f097bcfb46bda938e5a > 004.eml
  $ diff 001.eml 01.eml
  $ diff 002.eml 02.eml
  $ diff 003.eml 03.eml
  $ diff 004.eml 04.eml
