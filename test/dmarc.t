Tests on DMARC fields
  $ export BLAZE_DNS_STATIC=cache
  $ blaze dmarc verify --hostname omelet 001.mail -o 001.dmarc
  $ head -n8 001.dmarc
  Authentication-Results: omelet;
   spf=pass (omelet: domain of
    bounces+848413-e276-romain.calascibetta=gmail.com@sgmail.github.com
    designates 192.254.112.98 as permitted sender);
   dkim=pass header.i=@github.com header.s=s20150108 header.b=SuEKjwfk;
   dkim=pass header.i=@sendgrid.info header.s=smtpapi header.b=iIicLeoJ;
   dmarc=pass (p=REJECT sp=REJECT) header.from=github.com;
   
  $ blaze dmarc verify --hostname omelet 002.mail -o 002.dmarc
  $ head -n6 002.dmarc
  Authentication-Results: omelet;
   spf=pass (omelet: domain of noreply@github.com designates 192.30.252.192 as
    permitted sender);
   dkim=pass header.i=@github.com header.s=pf2014 header.b=1crXUDuJ;
   dmarc=pass (p=REJECT sp=REJECT) header.from=github.com;
   
  $ blaze dmarc verify --hostname omelet 003.mail -o 003.dmarc
  $ head -n6 003.dmarc
  Authentication-Results: omelet;
   spf=fail (omelet: domain of tbraibant@janestreet.com does not designates
    38.105.200.233 as permitted sender);
   dkim=pass header.i=@janestreet.com header.s=google header.b=MglJGvGH;
   dmarc=pass (p=QUARANTINE sp=QUARANTINE) header.from=janestreet.com;
   
  $ blaze dmarc verify --hostname omelet 004.mail -o 004.dmarc
  $ head -n7 004.dmarc
  Authentication-Results: omelet;
   spf=pass (omelet: domain of
    ocaml+verp-cdef9eaa098cd788943a4314c682b75a@discoursemail.com designates
    216.218.240.121 as permitted sender);
   dkim=fail header.i=@discoursemail.com header.s=sjc2 header.b=VmvuZ8wM;
   dmarc=pass (p=QUARANTINE sp=QUARANTINE) header.from=discoursemail.com;
   
