let re = [%re "/[ -]/"];

let abbreviate = s =>
  s
  |> Js.String.splitByRe(re)
  |> Array.map(Js.String.charAt(0))
  |> Array.map(Js.String.toUpperCase)
  |> Js.Array.joinWith("");
