Tests on SPF fields
  $ blaze.spf analyze 001.mail
  bounces+848413-e276-romain.calascibetta=gmail.com@sgmail.github.com from 192.254.112.98: pass (expected: pass)
  $ blaze.spf analyze 002.mail
  noreply@github.com from 192.30.252.192: pass (expected: pass)
  $ blaze.spf analyze 003.mail
  tbraibant@janestreet.com from 38.105.200.233: fail (expected: pass)
  $ blaze.spf stamp --ip 38.105.200.78 --sender tbraibant@janestreet.com < 003.mail > 003.diff
  $ diff 003.mail 003.diff
  0a1,4
  > Received-SPF: pass (omelet: domain of tbraibant@janestreet.com designates 38.105.200.78 as
  >  permitted sender) client-ip=38.105.200.78; envelope-from=
  >  tbraibant@janestreet.com; identity=mailfrom; receiver=omelet; mechanism=
  >  ip4:38.105.200.78/32;
  [1]

