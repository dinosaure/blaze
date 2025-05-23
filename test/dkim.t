Tests on DKIM fields
  $ export BLAZE_DNS_STATIC=cache
  $ blaze dkim verify 001.mail
  [OK]: sendgrid.info
  [OK]: github.com
  $ blaze dkim verify 002.mail
  [EX]: github.com
  $ blaze dkim verify 003.mail
  [OK]: janestreet.com
  $ blaze dkim verify -q 001.mail
  $ cat 002.mail | blaze dkim verify -q
  $ cat 002.mail | blaze dkim verify -q
  $ blaze rand --seed foo= 16 > seed
  $ blaze dkim gen --seed $(cat seed) key.pem | cut -d' ' -f3 > seed.out
  $ diff seed seed.out
  $ blaze dkim sign --seed $(cat seed) -s blaze -h x25519.org -f subject 001.mail > mail
  $ blaze dkim verify -q -e 'blaze:x25519.org:key.pem' mail
  $ blaze dkim gen --seed $(cat seed) | tail -n1 | cut -d' ' -f4 > pub
  $ blaze dkim sign --seed $(cat seed) -s blaze -h x25519.org 002.mail > mail
  $ blaze dkim verify -q -e 'blaze:x25519.org:'$(cat pub) mail
  $ blaze dkim sign --seed $(cat seed) -s blaze -h x25519.org -f received -f received 003.mail > mail
  $ blaze dkim verify --fields -e 'blaze:x25519.org:key.pem' mail
  From
  Received
  Received
  Mime-Version
  Date
  Message-Id
  Subject
  To
  Content-Type
  $ blaze dkim verify 004.mail
  [EX]: discoursemail.com
