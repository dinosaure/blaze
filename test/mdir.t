Tests on mdir executable
  $ export BLAZE_DNS_STATIC=cache
  $ NEW=$(blaze.mdir new -h blaze -D mdir | tail -n1)
  $ blaze.mdir get -h blaze -D mdir -o result --new $NEW
  $ diff result mdir/new/$NEW
  $ blaze.mdir get -h blaze -D mdir --new $NEW | blaze.dkim verify -
  [OK]: mcc.mcsv.net
  [OK]: mailchimpapp.net
  $ blaze.mdir get -h blaze -D mdir --new $NEW | blaze.spf analyze -
  bounce-mc.us11_46973437.796437-romain.calascibetta=gmail.com@mail148.suw18.rsgsv.net from 198.2.181.148: pass (expected: pass)
  $ export BLAZE_MDIR=mdir
  $ test -f mdir/new/$(blaze.mdir new -h blaze | tail -n1)
  $ blaze.mdir commit -h blaze -D mdir --new $NEW
  $ test -f mdir/cur/$NEW
