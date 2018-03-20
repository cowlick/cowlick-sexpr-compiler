type frame = <
  label: string Js.Nullable.t;
  scripts: Script.t array
> Js.t

type scene = <
  label: string;
  frames: frame array
> Js.t

type scenario = scene array
