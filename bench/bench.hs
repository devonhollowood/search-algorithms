import Criterion.Main
import Algorithm.Search

type Position = (Int, Int)

wall :: Int -> Position -> Bool
wall height (x, y) = x == 0 && abs y <= height

neighbors :: Position -> [Position]
neighbors (x, y) = [
    (x - 1, y),
    (x, y - 1),
    (x + 1, y),
    (x, y + 1)
  ]

dist :: Position -> Position -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

aStarWall :: Int -> Maybe (Int, [Position])
aStarWall height = aStar
    (neighbors `pruning` wall height) -- next
    (\_ _ -> 1) -- cost
    (dist end) -- remaining
    (== end) -- at end
    start -- initial
  where
    start = (-3, 0)
    end = (3, 0)

main :: IO ()
main = defaultMain [
    bgroup "aStar Wall Benchmark" [
      bench "Size 2 wall" $ nf aStarWall 2,
      bench "Size 4 wall" $ nf aStarWall 4,
      bench "Size 8 wall" $ nf aStarWall 8,
      bench "Size 16 wall" $ nf aStarWall 16,
      bench "Size 32 wall" $ nf aStarWall 32,
      bench "Size 64 wall" $ nf aStarWall 64
    ]
  ]
