Tests on ARC
  $ export BLAZE_DNS_STATIC=cache
  $ blaze arc verify 001.mail
  notifications@github.com
  $ blaze arc verify 002.mail
  notifications@github.com -✓-> 01:google.com
  $ blaze arc verify 004.mail
  ocaml@discoursemail.com -✓-> 01:google.com
  $ blaze arc verify 005.mail
  fntoth@gmail.com -✓-> 01:webhostingserver.nl -✓-> 02:webhostingserver.nl
    -✓-> 03:subspace.kernel.org
  $ blaze arc verify 006.mail
  mihail.atanassov@arm.com -✓-> 01:microsoft.com -✓-> 02:microsoft.com
    -🞩-> 03:subspace.kernel.org
