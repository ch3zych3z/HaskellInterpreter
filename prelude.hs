fst (a, _) = a;
snd (_, b) = b;

min a b = if a < b then a else b;
max a b = if a > b then a else b;

curry f a b = f (a, b);
uncurry f (a, b) = f a b;

foldr f z0 l = case l of
  { []     -> z0
  ; (x:xs) -> f x (foldr f z0 xs)
  };
foldl f z0 l = case l of
  { []     -> z0
  ; (x:xs) -> foldl f (f z0 x) xs
  };

head (h : _) = h;
tail (_ : t) = t;
last l = case l of
  { [x]    -> x
  ; (_:xs) -> last xs
  };

id x = x;
const a _ = a;

map f l = case l of
  { []     -> []
  ; (x:xs) -> f x : map f xs
  };

filter f l = case l of
  { []     -> []
  ; (x:xs) -> if f x then x : filter f xs else filter f xs
  };

length l = case l of
  { []     -> 0
  ; (_:xs) -> 1 + length xs
  };

zipWith f l1 l2 = case (l1, l2) of
  { ([], _) -> []
  ; (_, []) -> []
  ; ((x1:xs1), (x2:xs2)) -> f x1 x2 : zipWith f xs1 xs2
  };

repeat x = x : repeat x;

take n l = if n == 0 then [] else case l of
  { []     -> []
  ; (x:xs) -> x : take (n - 1) xs
  };

replicate n x = take n (repeat x);

not x = case x of { True -> False; False -> True }
