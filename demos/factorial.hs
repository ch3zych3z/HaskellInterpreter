f x = if x == 0 then 1 else x * f (x - 1);

main = print (f 5)
