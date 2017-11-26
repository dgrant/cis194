-- Move from first peg to third peg with second as temp storage
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 2 a b c = [(a, b), (a, c), (b, c)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, c)] ++ (hanoi (n-1) b a c)

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 2 a b c d = [(a, b), (a, d), (b, d)]
hanoi2 3 a b c d = [(a, b), (a, c), (a, d), (c, d), (b, d)]
hanoi2 n a b c d = (hanoi2 (n - 1) a b d d) ++ [(a, d)] ++ (hanoi2 (n-1) c d a b)

count :: [Move] -> Integer
count [] = 0
count (x:xs) = 1 + count xs

main = do
    print(count (hanoi 15 "a" "b" "c"))
    print(count (hanoi2 15 "a" "b" "c" "d"))
