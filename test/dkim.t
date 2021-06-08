Tests on DKIM fields
  $ export BLAZE_DNS=cache
  $ blaze.dkim verify 001.mail
  [OK]: sendgrid.info
  [OK]: github.com
  $ blaze.dkim verify 002.mail
  [WARNING][dkim]: The given DKIM-Signature expired.
  [EX]: github.com
  $ blaze.dkim verify 003.mail
  [OK]: janestreet.com
  $ blaze.dkim verify -q 001.mail
  $ cat 002.mail | blaze.dkim verify -q -n tcp://$(grep -m1 nameserver /etc/resolv.conf | cut -d' ' -f2) -
  $ cat 002.mail | blaze.dkim verify -q -n udp://$(grep -m1 nameserver /etc/resolv.conf | cut -d' ' -f2) -
  $ dd if=/dev/urandom bs=32 count=1 status=none | base64 - > seed
  $ blaze.dkim gen --seed $(cat seed) key.pem | cut -d' ' -f3 > seed.out
  $ diff seed seed.out
  $ blaze.dkim sign --seed $(cat seed) -s blaze -h x25519.org -f subject 001.mail > mail
  $ blaze.dkim verify -q -e 'blaze:x25519.org:key.pem' mail
  $ blaze.dkim gen --seed $(cat seed) | tail -n1 | cut -d' ' -f4 > pub
  $ blaze.dkim sign --seed $(cat seed) -s blaze -h x25519.org 002.mail > mail
  $ blaze.dkim verify -q -e 'blaze:x25519.org:'$(cat pub) mail
  $ blaze.dkim sign --seed $(cat seed) -s blaze -h x25519.org -f received -f received 003.mail > mail
  $ blaze.dkim verify --fields -e 'blaze:x25519.org:key.pem' mail
  From
  Received
  Received
  Mime-Version
  Date
  Message-Id
  Subject
  To
  Content-Type
  $ blaze.dkim verify 004.mail
  [WARNING][dkim]: The given DKIM-Signature expired.
  [EX]: discoursemail.com
