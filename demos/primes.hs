interval b = b : interval (b + 1);
nats = interval 1;

sieve (x : xs) = x : sieve (filter (\y -> not (y `mod` x == 0)) xs);

primes = sieve (tail nats);

main = print (take 5 primes)