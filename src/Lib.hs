module Lib where

-- for convenience
log2 = logBase 2

entropy :: [Double] -> Double
entropy probs
  = sum (map entropy' probs)
  where
    entropy' :: Double -> Double
    entropy' 0.0
      = 0.0
    entropy' prob
      = - prob * log2 prob

kraftMcMillanNumber :: [(Char, String)] -> Double
kraftMcMillanNumber codes
  = sum (map (\(_, encoded) -> 1.0 / fromIntegral (length encoded)) codes)

averageWordLength :: [(Double, String)] -> Double
averageWordLength codes
  = sum (map (\(prob, encoded) -> prob * fromIntegral (length encoded)) codes)
