import Data.List

data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

type Code = [Peg]

data Move = Move Code Int Int
          deriving (Show, Eq)

colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

exactMatches :: Code -> Code -> Int
exactMatches x y = length . filter (==True) $ zipWith (==) x y

countColors :: Code -> [Int]
countColors c = map (\x -> length (elemIndices x c)) colors

matches :: Code -> Code -> Int
matches x y = sum $ zipWith min (countColors x) (countColors y)

getMove :: Code -> Code -> Move
getMove secret code = Move code exact nonexact
    where exact    = exactMatches secret code
          nonexact = (matches secret code) - exact

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move code _ _) secret = getMove secret code == move

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = concatMap concatPrevCodes (allCodes 1)
    where concatPrevCodes codes = map (++ codes) (allCodes (n - 1))

solve :: Code -> [Move]
solve secret = doMoves (allCodes (length secret))
    where doMoves :: [Code] -> [Move]
          doMoves [] = []
          doMoves (code:codes) = move:(doMoves validMoves)
              where move = getMove secret code
                    validMoves = filterCodes move codes