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
  $ blaze pack get pack.pack 6c4886f24124c436bc04e23923e3bc508c6b90d6 > 001.eml
  $ blaze pack get pack.pack 92b2239506873e558b7f6d9a83ed2a82c17ad641 > 002.eml
  $ blaze pack get pack.pack 299e18f2598abe172368ed1be8cbcd29cbd9c847 > 003.eml
  $ blaze pack get pack.pack 6f61a31d9040d9587fc26a28c478fc78cdb7635d > 004.eml
  $ diff 001.eml 01.eml
  $ diff 002.eml 02.eml
  $ diff 003.eml 03.eml
  $ diff 004.eml 04.eml
