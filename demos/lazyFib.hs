zipWith' f x1 x2 =
    case x1 of
    { []       -> []
    ; (x : xs) ->
        case x2 of
        { []         -> []
        ; (x' : xs') -> f x x' : zipWith' f xs xs'
        }
    };

tail' (x : xs) = xs;

fib = 0 : 1 : zipWith' (\a b -> a + b) fib (tail' fib);

takeN n l = case l of
  { []       -> []
  ; (x : xs) -> if n == 0 then [] else x : takeN (n - 1) xs
  };

main = print (takeN 10 fib)