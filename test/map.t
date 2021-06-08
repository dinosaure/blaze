Tests isomorphism
  $ blaze.map -o 001.prim < 001.mail
  $ blaze.addr 001.prim > addr.prim
  $ blaze.addr 001.mail > addr.mail
  $ diff addr.prim addr.mail
  $ blaze.map -o 002.prim < 002.mail
  $ blaze.addr 002.prim > addr.prim
  $ blaze.addr 002.mail > addr.mail
  $ diff addr.prim addr.mail
  $ blaze.map -o 003.prim < 003.mail
  $ blaze.addr 003.prim > addr.prim
  $ blaze.addr 003.mail > addr.mail
  $ diff addr.prim addr.mail
