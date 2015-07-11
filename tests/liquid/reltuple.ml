let test pr =
  match pr with
    | (a, b) ->
        assert(a < b)

let x = 1
let y = 2
let p = (x, y)
let _ = test p
