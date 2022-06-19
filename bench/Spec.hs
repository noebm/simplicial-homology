
import Criterion.Main

import qualified Data.Matrix as M
import SmithNormalForm

rp2_boundarymap1 = M.fromList 6 15 [-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,-1,-1,-1,-1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,-1,-1,-1,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,-1,-1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,-1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1]

rp2_boundarymap2 = M.fromList 15 10 [1,1,0,0,0,0,0,0,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0,-1,0,0,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,-1,-1,0,0,0,0,1,0,0,0,0,0,-1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,-1,-1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,1]

f :: M.Matrix Integer -> M.Matrix Integer
f = smithNormalForm

main :: IO ()
main = defaultMain
  [ bgroup "snf"
    [ bench "m1" $ nf f (M.fromList 3 3 [2, 4, 4, -6, 6, 12, 10, 4, 16])
    , bench "m2" $ nf f (M.fromList 5 5 [ 25, -300, 1050, -1400, 630, -300, 4800, -18900, 26880, -12600, 1050, -18900, 79380, -117600, 56700, -1400, 26880, -117600, 179200, -88200, 630, -12600, 56700, -88200, 44100])
    , bench "m3" $ nf f (M.fromList 4 4 [4, 0, 0, 0, 0, 6, 0, 0, 0, 0, 8, 0, 0, 0, 0, 5])
    , bench "rp2_d1" $ nf f rp2_boundarymap1
    , bench "rp2_d2" $ nf f rp2_boundarymap2
    ]
  ]
