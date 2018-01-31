open Type
open Printf

exception Env_error of string
let error message = raise (Env_error message)

let lookup env name =
  try
    let r = env |> List.find (fun x -> x.name = name)
    in r.value
  with Not_found ->
    error (sprintf "not found in env: %s" name)

let set env name value =
  try
    match List.find (fun x -> x.name = name) !env with
      x -> x.value <- value
  with Not_found ->
    let symbol = { name = name ; value = value } in
    env := symbol :: !env

