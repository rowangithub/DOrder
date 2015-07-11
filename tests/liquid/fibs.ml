qualifier P(x) = 0 < x;;
qualifier N(x) = x < 0;;

let rec fibs n =
  if n = 0 then
    []
  else
    let p = fibs (n - 1) in
      if n <= 2 then
	1::p
      else
	match p with
            [] -> []
          | r::l ->
              match l with
                  [] -> []
                | s::m -> (r + s)::p
in fibs 10;;
