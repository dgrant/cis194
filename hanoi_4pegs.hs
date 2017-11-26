-- Move from first peg to third peg with second as temp storage
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi 2 a b c d = [(a, b), (a, d), (b, d)]
hanoi 3 a b c d = [(a, b), (a, c), (a, d), (c, d), (b, d)]
hanoi n a b c d = (hanoi (n - 1) a b d d) ++ [(a, d)] ++ (hanoi (n-1) c d a b)

main = do
    print(hanoi 6 "a" "b" "c" "d")
