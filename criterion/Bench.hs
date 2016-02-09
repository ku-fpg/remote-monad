import Criterion.Main

fib :: Int -> Int
fib n
  | n <= 0    = 1
  | otherwise = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain
  [ bgroup "fib"
      [ bench "3" $ whnf fib 3
      , bench "5" $ whnf fib 5
      , bench "7" $ whnf fib 7
      ]
  ]

