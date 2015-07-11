qual P(x): 0 < x;;
qual N(x): x < 0;;

? letrec mapfilter = fun f -> fun l ->
  match l with
      [] ->
	[]
    | h::t ->
	let r = mapfilter f t in
	let x = f h in
	  match x with
              [] ->
                r
            | z::e ->
                z::r
in
let pos = fun y ->
  if 0 < y then
    y::[]
  else
    []
in
let neg = fun w ->
  if w < 0 then
    w::[]
  else
    []
in
let i = mapfilter pos 1::2::-1::[] in
let h = mapfilter neg 1::2::-1::[] in
  3;;
