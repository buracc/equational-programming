module Practicum2B where

    {-
Name:           Burak Inan
VU-net id:      bin230
Student number: 2672032
Discussed with: 
Remarks:        
Sources:   See Practicum2A
 

-}
    
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


-- given: definition of the monad MaybeOne used for partial functions
data MaybeOne a = NoResult
                | Result a deriving (Show, Eq)

instance Applicative MaybeOne where
    pure x = Result x
    (<*>)  = ap

instance Functor MaybeOne where
    fmap = liftM

instance Monad MaybeOne where
  NoResult   >>= _ = NoResult
  (Result n) >>= f = f n
  return x         = Result x


-- Example: partial function for division
myDividedBy :: Double -> Double -> MaybeOne Double
myDividedBy n d =
  if d  == 0
  then NoResult
  else Result (n / d)

-- Exercise 1-a
myIndexOf :: [Double] -> Double -> MaybeOne Int
myIndexOf [] n = NoResult
myIndexOf (h:t) n = 
  myIndexOfAux (h:t) 1
  where
  myIndexOfAux (h:t) i =
    if (h == n) then 
      Result i
    else if length (h:t) < i then
      NoResult
    else
      myIndexOfAux t (i + 1)

-- Exercise 1-b
myRemainderString :: String -> String -> MaybeOne String
myRemainderString "" "" = NoResult
myRemainderString "" y = Result y
myRemainderString x "" = NoResult
myRemainderString (x:xs) (y:ys) =
  if (y /= x) then
    NoResult
  else myRemainderString xs ys
  

-- Create an operator for our divide function
n // d = n `myDividedBy` d

-- Example f using case (1/2)
f :: Double -> Double -> Double -> MaybeOne Double
f x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case xDividedByy // z of
        NoResult   -> NoResult
        Result   r -> Result r

-- Example f using case (2/2)
fShorter :: Double -> Double -> Double -> MaybeOne Double
fShorter x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy -> xDividedByy // z

-- Example g using case
g :: Double -> Double -> Double -> Double -> MaybeOne Double
g x y z s =
  case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case y // z of
        NoResult           -> NoResult
        Result xDividedByz ->
          case y // s of
            NoResult           -> NoResult
            Result yDividedBys ->
              case z // s of
                NoResult           -> NoResult
                Result zDividedBys ->
                  let n = yDividedBys + zDividedBys
                      d = xDividedByy - xDividedByz
                  in n // d


-- Exercise 2
v1 :: Double -> Double -> Double -> Double -> MaybeOne Double
v1 x y z s =
  case (myDividedBy x y) of 
    NoResult -> NoResult
    Result xdivy -> 
      case (myDividedBy z s) of 
        NoResult -> NoResult
        Result zdivs -> 
          case (myDividedBy y s) of
            NoResult -> NoResult
            Result ydivs -> 
              case (myDividedBy z x) of 
                NoResult -> NoResult
                Result zdivx -> 
                  case (myDividedBy xdivy (zdivs - ydivs)) of
                    NoResult -> NoResult
                    Result left -> 
                      let right = ydivs + zdivx
                        in Result (left - right)

-- Example f using >==
fBetter :: Double -> Double -> Double -> MaybeOne Double
fBetter x y z = (x // y) >>= dividedByZ
  where dividedByZ xdividedByy = xdividedByy // z

-- Example f using >= and lambda 
fBetterLambda :: Double -> Double -> Double -> MaybeOne Double
fBetterLambda x y z = (x // y) >>= (\xDividedByy -> xDividedByy // z)

-- Example g using >== and lambda
gBetter :: Double -> Double -> Double -> Double -> MaybeOne Double
gBetter x y z s =
  (x // y) >>=
  (\xDividedByy ->
    (x // z) >>=
    (\xDividedByz ->
      let d = xDividedByy - xDividedByz
      in (y // s) >>=
      (\yDividedBys ->
        (z // s) >>=
        (\zDividedBys ->
          let n = yDividedBys + zDividedBys
          in n // d
        )
      )
    )
  )

-- Exercise 3
v2 :: Double -> Double -> Double -> Double -> MaybeOne Double
v2 x y z s = 
  (myDividedBy x y) >>=
   (\xdivy -> 
      (myDividedBy z s) >>= 
        (\zdivs -> 
          (myDividedBy y s) >>=
            (\ydivs -> 
              (myDividedBy z x) >>= 
                (\zdivx -> 
                  (myDividedBy xdivy (zdivs - ydivs)) >>=
                    (\left -> 
                      Result (left - (ydivs + zdivx))
                    )
                )
            )
        )
   )

-- Example f using do
fDo :: Double -> Double -> Double -> MaybeOne Double
fDo x y z = do
  xDividedByy <- x // y
  xDividedByy // z

-- Example g using do
gDo :: Double -> Double -> Double -> Double -> MaybeOne Double
gDo x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let d = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let n = yDividedBys + zDividedBys
  n // d

-- Example f using do-return
fPerfect :: Double -> Double -> Double -> MaybeOne Double
fPerfect x y z = do
  xDividedByy <- x // y
  result      <- xDividedByy // z
  return result

-- Example g using do-return
gPerfect :: Double -> Double -> Double -> Double -> MaybeOne Double
gPerfect x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let denominator = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let numerator = yDividedBys + zDividedBys
  result <- numerator // denominator
  return result

-- Exercise 4
v3 :: Double -> Double -> Double -> Double -> MaybeOne Double
v3 x y z s = 
  do
    xdivy <- myDividedBy x y
    zdivs <- myDividedBy z s
    ydivs <- myDividedBy y s
    zdivx <- myDividedBy z x
    left <- myDividedBy xdivy (zdivs - ydivs)
    return (left - (ydivs + zdivx))
