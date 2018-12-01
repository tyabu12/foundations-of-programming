open Ex1702
open Ex1703

(* year_t 型の y を受け取り, seiza_t 型の星座を返す *)
(* seiza : year_t -> seiza_t *)
let seiza y = match y with
  | January d -> if d <= 19 then Capricornus else Aquarius
  | February d -> if d <= 18 then Aquarius else Pisces
  | March d -> if d <= 20 then Pisces else Aries
  | April d -> if d <= 19 then Aries else Taurus
  | May d -> if d <= 20 then Taurus else Gemini
  | June d -> if d <= 21 then Gemini else Cancer
  | July d -> if d <= 22 then Cancer else Leo
  | August d -> if d <= 22 then Leo else Virgo
  | September d -> if d <= 22 then Virgo else Libra
  | October d -> if d <= 23 then Libra else Scorpius
  | November  d -> if d <= 22 then Scorpius else Sagittarius
  | Decenber d -> if d <= 21 then Sagittarius else Capricornus

(* テスト *)
let () =
  assert (seiza (August 25) = Virgo);
  assert (seiza (May 3) = Taurus);
  ()
