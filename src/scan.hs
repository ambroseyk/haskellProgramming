fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fact = scanl (*) 1 [1 ..]

factorial n = fact !! n
