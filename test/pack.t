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
  $ blaze pack get pack.pack 58274dd7fa55cfbcad4e8506a4eef38372e4603b > 001.eml
  $ blaze pack get pack.pack 6f56bd209555cc217d0ba3a0f23099c33b9d438e > 002.eml
  $ blaze pack get pack.pack 2f630dd27d847c56e51ea7b3528f63962f896c04 > 003.eml
  $ blaze pack get pack.pack 97e4bcb4d4a18decccb1025ea3eab896da340df4 > 004.eml
  $ diff 001.eml 01.eml
  $ diff 002.eml 02.eml
  $ diff 003.eml 03.eml
  $ diff 004.eml 04.eml
  $ blaze pack list pack.pack
  0000000c 58274dd7fa55cfbcad4e8506a4eef38372e4603b
  000013be 6f56bd209555cc217d0ba3a0f23099c33b9d438e
  00002c92 2f630dd27d847c56e51ea7b3528f63962f896c04
  000036d5 97e4bcb4d4a18decccb1025ea3eab896da340df4
  $ blaze pack delete pack.pack 9afdd2f7a07de00e5de93bc1f706a2ab2f5caee9
  $ rm pack.idx
  $ blaze pack list pack.pack
  0000000c 58274dd7fa55cfbcad4e8506a4eef38372e4603b
  000013be 6f56bd209555cc217d0ba3a0f23099c33b9d438e
  00002c92 2f630dd27d847c56e51ea7b3528f63962f896c04
  000036d5 97e4bcb4d4a18decccb1025ea3eab896da340df4
