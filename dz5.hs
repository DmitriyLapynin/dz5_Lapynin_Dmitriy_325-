pythTriples :: [(Int, Int, Int)]
pythTriples = [(x, y, z)  | x <- [1..10],
                                    y <- [1..10],
                                    z <- [1..10], ((x^2 + y^2) == z^2)]

primes :: [Int]
primes = [x | x <- [2..100], length [y | y <- [2..x-1], mod x y == 0] == 0] 


data Stream a = StreamC a (Stream a) deriving Show

ones :: Stream Int
ones = StreamC 1 ones

zipStream :: Stream a -> Stream b -> Stream (a, b)
zipStream (StreamC x xs) (StreamC y ys) = (StreamC (x, y)) (zipStream xs ys)

zipStreamWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreamWith f (StreamC x xs) (StreamC y ys) = StreamC (f x y) (zipStreamWith f xs ys)


fibStream :: Stream Int -> Stream Int
fibStream (StreamC x (StreamC y xs)) = (StreamC (x + y) (fibStream (StreamC y (StreamC (x + y) xs))))

