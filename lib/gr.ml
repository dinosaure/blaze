include Graph.Imperative.Digraph.Concrete (struct
  include Mrmime.MessageID

  let compare a b =
    let v = Emile.compare_local ~case_sensitive:true (fst a) (fst b) in
    if v = 0
    then Emile.compare_domain (snd a :> Emile.domain) (snd b :> Emile.domain)
    else v

  let hash = Hashtbl.hash
end)
