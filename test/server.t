Test on the simple server
  $ export PORT=4242
  $ blaze.srv 127.0.0.1:$PORT -o new.eml &
  $ cat >old.eml <<EOF
  > From: admin@blaze.org
  > To: foo@bar
  > Subject: Hello Blaze!
  > 
  > Hello fellow!
  > EOF
  $ cat old.eml | blaze.send --sender admin@blaze.org -r foo@bar - 127.0.0.1:$PORT
  $ tr -d '\r' < new.eml > new_without_crlf.eml
  $ diff old.eml new_without_crlf.eml
