type ruby = <
  rb: string;
  rt: string
> Js.t

let to_json r = Printf.sprintf "{\"rb\":\"%s\",\"rt\":\"%s\"}" r##rb r##rt

let half = Js.Re.fromString {js|[u21-u7e\\sｦ-ﾟ]|js}

let unicode = [%re {|/[\uD800-\uDBFF][\uDC00-\uDFFF]|[^\uD800-\uDFFF]/g|}]

let split text =
  text
  |> Js.String.match_ unicode
  |> Js.Option.getWithDefault [||]

let to_whitespace n xs =
  xs
  |> Js.Array.sliceFrom n
  |> Js.Array.map (fun x -> if Js.Re.test x half then " " else {js|　|js})
  |> Js.Array.joinWith ""

let length text =
  text
  |> Js.String.match_ unicode
  |> Js.Option.getWithDefault [||]
  |> Js.Array.length

let generate rb rt =
  let bs = split rb in
  let bl = Js.Array.length bs in
  let ts = split rt in
  let tl = Js.Array.length ts in
  let result = [||] in
  if tl = 0 then result
  else begin
    let d = float bl /. float tl in
    let len =
      if bl = tl then (fun l -> l)
      else (fun l -> Js.Math.ceil (float l /. d)) in
    bs
    |> Array.fold_left (fun (s, i) x ->
      let b = s ^ x in
      let j = i + 1 in
      let bw = to_whitespace j bs in
      let l = Js.String.length b in
      let t = Js.Array.joinWith "" (Js.Array.slice ~start:0 ~end_:(len l) ts) in
      let w = to_whitespace (length t) ts in
      result
      |> Js.Array.push [%bs.obj {
        rb = b ^ bw;
        rt = t ^ w
      }]
      |> ignore;
      (b, j)
    ) ("", 0)
    |> ignore;
    result
  end

