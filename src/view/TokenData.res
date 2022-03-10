type t = string

let duplicate = t => t

let size = t => {"width": String.length(t)->(f => f * 10)->Int.toFloat, "height": 20.}
let render = t =>
  <text x={"50%"} y={"50%"} textAnchor="middle" dominantBaseline="central">
    {React.string(t)}
  </text>
