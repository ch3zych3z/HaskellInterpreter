listOfMins list = let
{ helper l =
    case l of
    { [x]    -> ([m], x)
    ; (x:xs) -> let
      { tmp = helper xs
      ; curMin = snd tmp
      ; curAns = fst tmp
      } in (m : curAns, min curMin x)
    }
; ans = helper list
; m = snd ans
; minList = fst ans
} in minList;

main = print (listOfMins [1,2,5,12,2,0,3,15])
