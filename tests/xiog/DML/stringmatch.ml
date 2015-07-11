type rpattern =
  Rnil | Remp | Rcha of char
| Rplu of rpattern * rpattern
| Rtim of rpattern * rpattern
| Rsta of rpattern
;;

datatype pattern with (nat,nat) =
  Pnil (0, 1) | Pemp(0, 0)
| Pcha(0, 1) of char
| {s1:nat}{s2:nat}{p1:nat | p1 > 0}{p2:nat | p2 > 0}
  Pplu(1+s1+s2, min(p1, p2)) of pattern(s1, p1) * pattern(s2, p2)
| {s1:nat}{s2:nat}{p1:nat}{p2:nat | p1+p2 > 0}
  Ptim(1+s1+s2, p1+p2) of pattern(s1, p1) * pattern(s2, p2)
|  {s:nat}{p:nat | p > 0} Psta(1+s, 0) of pattern(s, p)
;;

let rec delta = function
    Rnil -> Pnil
  | Remp -> Pemp
  | Rcha _ -> Pnil
  | Rplu (rp1, rp2) -> begin
      match delta rp1, delta rp2 with
	Pnil, Pnil -> Pnil
      |	_, _ -> Pemp
    end
  | Rtim (rp1, rp2) -> begin
      match delta rp1, delta rp2 with
	Pemp, Pemp -> Pemp
      |	_, _ -> Pnil
    end
  | Rsta rp -> Pemp
withtype rpattern -> pattern
;;

let rec norm = function
    Rnil -> Pnil
  | Remp -> Pnil
  | Rcha c -> Pcha c
  | Rplu (rp1, rp2) -> Pplu (norm rp1, norm rp2)
  | Rtim (rp1, rp2) ->
      let p1 = norm rp1 in
      let p2 = norm rp2
      in Pplu (Pplu (Ptim (delta rp1, p2),
		     Ptim (p1, delta rp2)),
	       Ptim (p1, p2))
  | Rsta rp -> let p = norm rp in Ptim (p, Psta p)
withtype rpattern -> [s:nat][p:nat | p > 0] pattern(s,p)
;;

let rec acc p cs k =
  match p with  
    Pnil -> false
  | Pemp -> k (cs)
  | Pcha(c') -> begin
    match cs with
    | [] -> false
    | c :: cs' -> if (c = c') then k (cs') else false
    end
  | Pplu(p1, p2) -> if acc p1 cs k then true else acc p2 cs k
  | Ptim(p1, p2) -> acc p1 cs (fun cs' -> acc p2 cs' k)
  | Psta(p0) -> if k (cs) then true else acc p0 cs (fun cs' -> acc p cs' k)
withtype {s:nat}{p:nat}{n:nat}
         pattern(s,p) -> char list(n) ->
         ({n':nat | n'+p <= n} char list(n') -> bool) -> bool
;;

let accept rp cs =
  let k0 = (function [] -> true | _ :: _ -> false)
  in if (acc (delta rp) cs k0) then true else acc (norm rp) cs k0
withtype rpattern -> char list -> bool
;;
