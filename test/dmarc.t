Tests on DMARC fields
  $ export BLAZE_DNS_STATIC=cache
  $ blaze dmarc verify --hostname omelet 001.mail -o 001.dmarc
  $ blaze dmarc collect 001.dmarc
         omelet: ✓ spf
                 ✓ dkim (header.i=@github.com header.s="s20150108"
                           header.b="SuEKjwfk")
                 ✓ dkim (header.i=@sendgrid.info header.s="smtpapi"
                           header.b="iIicLeoJ")
                 ✓ dmarc (header.from="github.com")
  mx.google.com: ✓ dkim (header.i=@github.com)
                 ✓ dkim (header.i=@sendgrid.info)
                 ✓ spf (smtp.mailfrom=bounces+848413-e276-romain.calascibetta=gmail.com@sgmail.github.com)
                 ✓ dmarc (header.from="github.com")
  $ blaze dmarc verify --hostname omelet 002.mail -o 002.dmarc
  $ blaze dmarc collect 002.dmarc
         omelet: ✓ spf
                 ✓ dkim (header.i=@github.com header.s="pf2014"
                           header.b="1crXUDuJ")
                 ✓ dmarc (header.from="github.com")
  mx.google.com: ✓ dkim (header.i=@github.com header.s="pf2014"
                           header.b="1crXUDuJ")
                 ✓ spf (smtp.mailfrom=noreply@github.com)
                 ✓ dmarc (header.from="github.com")
  $ blaze dmarc verify --hostname omelet 003.mail -o 003.dmarc
  $ blaze dmarc collect 003.dmarc
         omelet: 🞩 spf
                 ✓ dkim (header.i=@janestreet.com header.s="google"
                           header.b="MglJGvGH")
                 ✓ dmarc (header.from="janestreet.com")
  mx.google.com: ✓ spf (smtp.mailfrom=tbraibant@janestreet.com)
                 ✓ dkim (header.i=@janestreet.com)
  $ blaze dmarc verify --hostname omelet 004.mail -o 004.dmarc
  $ blaze dmarc collect 004.dmarc
         omelet: ✓ spf
                 🞩 dkim (header.i=@discoursemail.com header.s="sjc2"
                            header.b="VmvuZ8wM")
                 ✓ dmarc (header.from="discoursemail.com")
  mx.google.com: ✓ dkim (header.i=@discoursemail.com header.s="sjc2"
                           header.b="VmvuZ8wM")
                 ✓ spf (smtp.mailfrom=ocaml+verp-cdef9eaa098cd788943a4314c682b75a@discoursemail.com)
                 ✓ dmarc (header.from="discoursemail.com")
