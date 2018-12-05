--testings for the Grains module

module TestGrains where

import Grains 

-- if you were serious about this, quickcheck might be useful

emptyGrain = Grain {grainLength=0, contents=[], startTime=0}

onesGrain = Grain {grainLength=10, contents=replicate 10 1, startTime=0}
onesGrainOfBySome = Grain {grainLength=10, contents=replicate 10 1, startTime=8}



-- contents $ mergeGrains onesGrain onesGrainOfBySome 
-- grainLength $ mergeGrains emptyGrain onesGrain 


-- contents $ foldr mergeGrains emptyGrain [onesGrain, onesGrainOfBySome]

-- foldr mergeGrains emptyGrain [onesGrain, onesGrainOfBySome] 
--   == mergeGrains onesGrain onesGrainOfBySome 



tensGrain = Grain {grainLength=10, contents=replicate 10 10, startTime=0}

-- linearEnvelope 5 $ contents tensGrain 


-- old slow version of mergeGrains
-- mergeGrains :: Grain -> Grain -> Grain
-- mergeGrains g1 g2 =
  -- let
  --   (pre1, pre2) = 
  --     if startTime g1 > startTime g2
  --       then (startTime g1 - startTime g2, 0)
  --       else (0, startTime g2 - startTime g1)
  --   (n1, n2) =
  --     if (startTime g1 + grainLength g1 > startTime g2 + grainLength g2)
  --       then (0, startTime g1 + grainLength g1 - startTime g2 + grainLength g2)
  --       else (startTime g2 + grainLength g2 - startTime g1 + grainLength g1, 0)
  --   newContents = 
  --     zipWith (+) 
  --       (replicate pre1 0 ++ contents g1 ++ replicate n1 0) 
  --       (replicate pre2 0 ++ contents g2 ++ replicate n2 0)
  --   newStart = min (startTime g1) (startTime g2)
  --   newLength = length newContents
  -- in 
  --   Grain {startTime=newStart, grainLength=newLength, contents=newContents}