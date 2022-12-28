sharp x a = \i -> if i == 0 then x else a(i-1);

forsome p = p(find(\a -> p a));
forevery p = not(forsome(\a -> not(p a)));

zero = Nothing;
one = Just 1;

find p =
  if forsome(\a -> p(sharp zero a)) then
    sharp zero (find(\a -> p(sharp zero a)))
  else
    sharp one (find(\a -> p(sharp one a)));

search p = if forsome(\a -> p a) then Just(find(\a -> p a)) else Nothing;

equal f g = forevery(\a -> f a == g a);

coerce x = case x of { Nothing -> 0; _ -> 1 };

f a = coerce (a 0) + coerce(a 1);
g a = coerce(a 3);

main = print [equal f g, equal f f, equal g g]
