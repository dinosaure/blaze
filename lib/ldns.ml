open Rresult

let src = Logs.Src.create "local-dns"

module Log = (val Logs.src_log src : Logs.LOG)

type record = Record : ('a Dns.Rr_map.rr * 'a) -> record
type local = record list Domain_name.Map.t

let rec assoc : type a. a Dns.Rr_map.rr -> record list -> a =
 fun record lst ->
  match (record, lst) with
  | Dns.Rr_map.Mx, Record (Mx, v) :: _ -> v
  | Dns.Rr_map.A, Record (A, v) :: _ -> v
  | Dns.Rr_map.Aaaa, Record (Aaaa, v) :: _ -> v
  | Dns.Rr_map.Txt, Record (Txt, v) :: _ -> v
  | _, [] -> raise Not_found
  | _, _ :: lst -> assoc record lst

exception Invalid_line of string

let mx (v, preference) =
  {
    Dns.Mx.preference = int_of_string preference;
    Dns.Mx.mail_exchange = Domain_name.(host_exn (of_string_exn v));
  }

let is_colon = ( = ) ':'

let parse_line line =
  match Astring.String.cut ~sep:":" line with
  | Some ("txt", str) ->
      Record (Dns.Rr_map.Txt, (0l, Dns.Rr_map.Txt_set.singleton str))
  | Some ("a", ipv4s) ->
      let lst = Astring.String.cuts ~sep:" " ipv4s in
      let lst = List.map Ipaddr.V4.of_string_exn lst in
      let set = Ipaddr.V4.Set.of_list lst in
      Record (Dns.Rr_map.A, (0l, set))
  | Some ("aaaa", ipv6s) ->
      let lst = Astring.String.cuts ~sep:" " ipv6s in
      let lst = List.map Ipaddr.V6.of_string_exn lst in
      let set = Ipaddr.V6.Set.of_list lst in
      Record (Dns.Rr_map.Aaaa, (0l, set))
  | Some ("mx", mxs) ->
      let lst = Astring.String.cuts ~sep:" " mxs in
      let lst = List.map (Astring.String.span ~sat:is_colon) lst in
      let lst = List.map mx lst in
      Record (Dns.Rr_map.Mx, (0l, Dns.Rr_map.Mx_set.of_list lst))
  | _ -> raise (Invalid_line line)

let parse_line line =
  try parse_line line with
  | Invalid_line _ as exn -> raise exn
  | _ -> raise (Invalid_line line)

let of_fpath local fpath =
  if Fpath.get_ext fpath = ".dns"
  then
    let domain_name = Fpath.(basename (rem_ext fpath)) in
    match Domain_name.of_string domain_name with
    | Error _ ->
        Log.warn (fun m -> m "%a is not a valid DNS cache file." Fpath.pp fpath) ;
        R.error_msgf "Invalid filename as a DNS cache: %a" Fpath.pp fpath
    | Ok domain_name ->
        let ic = open_in (Fpath.to_string fpath) in
        let rec go acc =
          match input_line ic |> parse_line with
          | value -> go (value :: acc)
          | exception End_of_file -> List.rev acc
          | exception Invalid_line _ -> go acc in
        let records = go [] in
        R.ok (Domain_name.Map.add domain_name records local)
  else R.error_msgf "Invalid filename as a DNS cache: %a" Fpath.pp fpath

let of_directory directory =
  let fold fpath local =
    match of_fpath local fpath with
    | Ok local ->
        Log.debug (fun m ->
            m "%a added into the local DNS cache." Fpath.pp fpath) ;
        local
    | Error _ ->
        Log.warn (fun m -> m "%a is ignored." Fpath.pp fpath) ;
        local in
  Bos.OS.Dir.fold_contents ~elements:`Files ~dotfiles:true ~traverse:`None fold
    Domain_name.Map.empty directory

let getaddrinfo :
    type a.
    local ->
    a Dns.Rr_map.rr ->
    'v Domain_name.t ->
    (a, [> `Msg of string ]) result =
 fun local record domain_name ->
  match
    Domain_name.Map.find (Domain_name.raw domain_name) local
    |> Option.get
    |> assoc record
  with
  | v -> Ok v
  | exception _ -> R.error_msgf "record does not exist locally"

type t = { dns : Dns_client_unix.t; local : local }

let create ?size ?nameservers ?timeout ?(local = Domain_name.Map.empty) stack =
  let dns = Dns_client_unix.create ?size ?nameservers ?timeout stack in
  { dns; local }

let getaddrinfo :
    type a.
    t -> a Dns.Rr_map.rr -> 'v Domain_name.t -> (a, [> `Msg of string ]) result
    =
 fun t record domain_name ->
  match getaddrinfo t.local record domain_name with
  | Ok _ as v -> v
  | Error _ -> Dns_client_unix.getaddrinfo t.dns record domain_name

let gethostbyname { local; dns } domain_name =
  match
    Domain_name.Map.find (Domain_name.raw domain_name) local
    |> Option.get
    |> assoc Dns.Rr_map.A
  with
  | _, vs -> Ok (Ipaddr.V4.Set.choose vs)
  | exception _ -> Dns_client_unix.gethostbyname dns domain_name

let gethostbyname6 { local; dns } domain_name =
  match
    Domain_name.Map.find (Domain_name.raw domain_name) local
    |> Option.get
    |> assoc Dns.Rr_map.Aaaa
  with
  | _, vs -> Ok (Ipaddr.V6.Set.choose vs)
  | exception _ -> Dns_client_unix.gethostbyname6 dns domain_name

let get_resource_record :
    type a.
    local ->
    a Dns.Rr_map.rr ->
    'v Domain_name.t ->
    ( a,
      [> `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ] )
    result =
 fun local record domain_name ->
  let domain_name = Domain_name.raw domain_name in
  match
    Domain_name.Map.find domain_name local |> Option.get |> assoc record
  with
  | v -> Ok v
  | exception _ -> R.error_msgf "record does not exist locally"

let get_resource_record :
    type a.
    t ->
    a Dns.Rr_map.rr ->
    'v Domain_name.t ->
    ( a,
      [> `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ] )
    result =
 fun t record domain_name ->
  match get_resource_record t.local record domain_name with
  | Ok _ as v -> v
  | Error _ -> Dns_client_unix.get_resource_record t.dns record domain_name
