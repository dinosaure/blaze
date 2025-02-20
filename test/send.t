Test on send command
  $ blaze send --verbosity=error -h omelet --sender admin@blaze.org -r romain.calascibetta@gmail.com - - <<EOF
  > From: admin@blaze.org
  > To: romain.calascibetta@gmail.com
  > Subject: Hello Blaze!
  > 
  > Hello fellow!
  > EOF
  EHLO omelet
  MAIL FROM:<admin@blaze.org>
  RCPT TO:<romain.calascibetta@gmail.com>
  DATA
  From: admin@blaze.org
  To: romain.calascibetta@gmail.com
  Subject: Hello Blaze!
  
  Hello fellow!
  .
  QUIT
  $ blaze send --verbosity=error -h omelet --sender foo@bar -r a@foo -r b@foo - - <<EOF
  > EOF
  EHLO omelet
  MAIL FROM:<foo@bar>
  RCPT TO:<a@foo>
  RCPT TO:<b@foo>
  DATA
  .
  QUIT
