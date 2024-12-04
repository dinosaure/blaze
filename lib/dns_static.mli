type t
type record = Record : ('a Dns.Rr_map.rr * 'a) -> record
type local = record list Domain_name.Map.t

type error =
  [ `Msg of string
  | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
  | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

val getaddrinfo :
  t ->
  'response Dns.Rr_map.key ->
  'a Domain_name.t ->
  ('response, [> `Msg of string ]) result

val gethostbyname :
  t -> [ `host ] Domain_name.t -> (Ipaddr.V4.t, [> `Msg of string ]) result

val gethostbyname6 :
  t -> [ `host ] Domain_name.t -> (Ipaddr.V6.t, [> `Msg of string ]) result

val get_resource_record :
  t ->
  'response Dns.Rr_map.key ->
  'a Domain_name.t ->
  ('response, [> error ]) result

val of_directory : Fpath.t -> (local, [> `Msg of string ]) result

val create :
  ?cache_size:int ->
  ?edns:[ `Auto | `Manual of Dns.Edns.t | `None ] ->
  ?nameservers:Dns.proto * Dns_client_miou_unix.Transport.io_addr list ->
  ?timeout:int64 ->
  ?local:local ->
  Happy_eyeballs_miou_unix.t ->
  t
