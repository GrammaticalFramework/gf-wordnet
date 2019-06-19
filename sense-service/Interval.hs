module Interval(Interval,union,intersection,size) where

type Interval a = [(a,a)]

union,intersection :: (Ord a, Enum a) => Interval a -> Interval a -> Interval a

union []           ys           = ys
union xs           []           = xs
union ((x1,x2):xs) ((y1,y2):ys) =
  if x1 < y1
    then if x2 < pred y1
           then (x1,x2) : union xs ((y1,y2):ys)
           else if x2 <= y2
                  then union xs ((x1,y2):ys)
                  else union ((x1,x2):xs) ys
    else if x1 <= succ y2
           then if x2 <= y2
                  then union xs ((y1,y2):ys)
                  else union ((y1,x2):xs) ys
           else (y1,y2) : union ((x1,x2):xs) ys

intersection []           ys           = []
intersection xs           []           = []
intersection ((x1,x2):xs) ((y1,y2):ys) =
  if x1 < y1
    then if x2 < y1
           then intersection xs ((y1,y2):ys)
           else if x2 <= y2
                  then (y1,x2) : intersection xs ((y1,y2):ys)
                  else (y1,y2) : intersection ((x1,x2):xs) ys
    else if x1 <= y2
           then if x2 <= y2
                  then (x1,x2) : intersection xs ((y1,y2):ys)
                  else (x1,y2) : intersection ((x1,x2):xs) ys
           else intersection ((x1,x2):xs) ys

size :: Enum a => Interval a -> Int
size [] = 0
size ((x1,x2):xs) = fromEnum x2-fromEnum x1+1 + size xs
