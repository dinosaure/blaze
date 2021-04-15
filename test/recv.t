Tests on received fields
  $ blaze.recv extract 001.mail
  from:github-smtp2b-ext-cp1-prd.iad.github.net -> by:ismtpd0004p1iad1.sendgrid.net -> for:<romain.calascibetta@gmail.com>
  	with ESMTP
  by:10.103.97.5
  	with SMTP
  from:o3.sgmail.github.com -> by:mx.google.com -> for:<romain.calascibetta@gmail.com>
  	with ESMTPS
  $ blaze.recv extract 002.mail
  from:out-1.smtp.github.com -> by:mx.google.com -> for:<romain.calascibetta@gmail.com>
  	with ESMTPS
  by:2002:a0c:8b6e::
  	with SMTP
  $ blaze.recv extract 003.mail
  by:10.31.135.131
  	with HTTP
  from:tot-qpr-mailcore2.delacy.com -> by:mxout4.mail.janestreet.com -> for:<romain.calascibetta@gmail.com>
  	with esmtps
  from:mail-vk0-f48.google.com -> by:mxgoog1.mail.janestreet.com -> for:<romain.calascibetta@gmail.com>
  	with esmtps
  by:10.36.56.134
  	with SMTP
  by:tot-qpr-mailcore2
  	with JS-mailcore
  from:mxout4.mail.janestreet.com -> by:mx.google.com -> for:<romain.calascibetta@gmail.com>
  	with ESMTPS
  by:vkex70 -> for:<romain.calascibetta@gmail.com>
  	with SMTP
  $ echo "To: romain.calascibetta@gmail.com\n" > mail
  $ cat mail | blaze.recv stamp -f smtp.google.com romain.calascibetta@gmail.com -h omelet - | blaze.recv extract
  from:smtp.google.com -> by:omelet -> for:<romain.calascibetta@gmail.com>
  	with UTF8LMTP
  	via UUCP
