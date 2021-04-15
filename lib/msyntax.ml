type index -
  | Subthread
  | Curthread
  | Parthread
  | Current of int

type seq =
  | Filename of Fpath.t
  | Index of Fpath.t * int
  | Stdin

type t =
  | Leaf of seq * index option
  | Node of t * index

let pp_number ppf n =
  if n >= 0
  then Fmt.pf ppf "+%d" n
  else Fmt.pf ppf "-%d" n

let pp_index ppf = function
  | Current 0 -> Fmt.pf ppf "."
  | Current n -> Fmt.pf ppf ".%a" pp_number n
  | Parthread -> Fmt.pf ppf "^"
  | Curthread -> Fmt.pf ppf "="
  | Subthread -> Fmt.pf ppf "_"

let pp_seq ppf = function
  | Filename v -> Fpath.pp ppf v
  | Index (maildir, n) -> Fmt.pf ppf "%a%a" Fpath.pp maildir pp_number n
  | Stdin -> Fmt.string ppf "-"

let rec pp ppf = function
  | Leaf (Index (_, -1), None) -> Fmt.pf ppf "$"
  | Leaf (Stdin, None) -> Fmt.pf ppf "-"
  | Leaf (seq, None) -> Fmt.pf ppf "%a" pp_seq seq
  | Leaf (seq, Some index) -> Fmt.pf ppf "%a%a" pp_seq seq pp_index index
  | Node (t, index) -> Fmt.pf ppf "%a%a" pp t pp_index index
