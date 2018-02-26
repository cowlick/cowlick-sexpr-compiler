type 'a t = {
  kind: 'a;
  loc: Location.t
}

let kind_of ast =
  let {kind} = ast in
  kind
