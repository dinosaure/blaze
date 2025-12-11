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
  $ blaze pack get pack.pack ccfebcbe87d582eb6897f526ab98d40827398ddd > 001.eml
  $ blaze pack get pack.pack 9afdd2f7a07de00e5de93bc1f706a2ab2f5caee9 > 002.eml
  $ blaze pack get pack.pack 2f630dd27d847c56e51ea7b3528f63962f896c04 > 003.eml
  $ blaze pack get pack.pack 573847dc0ca288f10a37da35e71a6292cd1e1aba > 004.eml
  $ diff 001.eml 01.eml
  $ diff 002.eml 02.eml
  $ diff 003.eml 03.eml
  $ diff 004.eml 04.eml
