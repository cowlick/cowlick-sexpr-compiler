type 'a t = {
  kind: 'a;
  loc: Location.t
}

let kind_of ast =
  let {kind} = ast in
  kind

let loc_of ast =
  let {loc} = ast in
  loc
