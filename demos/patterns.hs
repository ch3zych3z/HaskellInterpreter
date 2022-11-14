f true@True x = true == x;

g = f True;

h a@b@c@1 d@2 x = g (a + b + c + d == x);

main = print (h 1 2 6)