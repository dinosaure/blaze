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
  $ blaze rand --seed foo= 16 > seed
  $ blaze dkim gen --seed $(cat seed) key.pem | cut -d' ' -f3 > seed.out
  $ diff seed seed.out
  $ blaze dkim gen --seed $(cat seed) | tail -n1 | cut -d' ' -f4 > pub
  $ blaze arc sign --seed $(cat seed) --seal-selector blaze --signature-selector blaze -h x25519.net 004.mail > mail
  $ blaze arc verify -e 'blaze:x25519.net:'$(cat pub) mail
  ocaml@discoursemail.com -✓-> 01:google.com -✓-> 02:x25519.net
