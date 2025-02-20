  $ blaze crlf 001.mail > 001.crlf
  $ blaze iso 001.crlf > 001.new
  $ diff 001.crlf 001.new
  $ blaze crlf 002.mail > 002.crlf
  $ blaze iso 002.crlf > 002.new
  $ diff 002.crlf 002.new
  $ blaze crlf 003.mail > 003.crlf
  $ blaze iso 003.crlf > 003.new
  $ diff 003.crlf 003.new
  $ blaze crlf 004.mail > 004.crlf
  $ blaze iso 004.crlf > 004.new
  $ diff 004.crlf 004.new
