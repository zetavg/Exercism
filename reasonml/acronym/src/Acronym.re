let re = [%re "/[ -]/"];

let abbreviate = string =>
  string
  |> Js.String.splitByRe(re)
  |> Array.map(s => s |> Js.String.charAt(0) |> Js.String.toUpperCase)
  |> Js.Array.joinWith("");
