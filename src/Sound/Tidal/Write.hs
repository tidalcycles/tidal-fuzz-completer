module Sound.Tidal.Write where

import Sound.Tidal.TypesPrime
import Control.Monad

walkOut = do
              string <- wWalk $ Sig [] $ Pattern Osc
              return (show string ++ "\n\n")


replicateWalk = do
                  -- to do:: controllable number of iterations
                  -- iterations <- getLine
                  -- let iter = rInt iterations
                  let iter = 3
                  out <- replicateM iter walkOut -- replicateM_ vs replicate M?
                  return (out)


rInt :: String -> Int
rInt = read


writeText = do
              walks <- replicateWalk
              -- return (walks)
              writeFile "example.txt" (join [walks!!0, walks!!1, walks!!2])
