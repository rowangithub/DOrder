qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier NE0(x) = not(0 = x);;
qualifier LTs(x) = x < size arr;;
qualifier LEs(x) = x <= size arr;;
qualifier NEs(x) = not(x = size arr);;
qualifier LTss(x) = x+1 < size arr;;

qualifier LTss(x) = x < size vec;;
qualifier LEss(x) = x <= size vec;;
qualifier NEss(x) = not(x = size vec);;
qualifier EQss(x) = x = size arr;;
qualifier EQs(x) = x = size vec;;

qualifier NW(x) = not(size x = size vec);;
qualifier W(x) = size x = size vec;;
qualifier Z(x) = size x = sz_plus;;
qualifier Y(x) = sz < size x;;
qualifier ASDF(x) = 1 < size x;;
qualifier O(x) = 1 < sz;;

qualifier GE1(x) = 1 <= x;;
qualifier GT1(x) = 1 < x;;
qualifier NE1(x) = not(1 = x);;
qualifier EQ1(x) = x = 1;;
qualifier EQ0(x) = x = 0;;

qualifier LTsz(x) = x < sz;;
qualifier LEsz(x) = x <= sz_plus;;
qualifier GTsz(x) = sz < x;;
qualifier GEsz(x) = sz <= x;;
qualifier SZp(x) = x = sz+1;;
qualifier EQsz(x) = x = sz;;
qualifier NEsz(x) = not(x = sz);;

let yarr arr y = 
	let getarr i = get arr i in
	let y_plus = y + 1 in
	getarr y_plus
in
let sz = get_pos_int () in
let sz_plus = sz + 1 in
let z = 0 in
let vec = make sz_plus 0 in
yarr vec 0;;

let y = 1 in y;;

let x = 0 in let y = x + 1 in y;;
