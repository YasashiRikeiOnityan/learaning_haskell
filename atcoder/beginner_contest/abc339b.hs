import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)

data Color = White | Black deriving (Show, Eq)
data Direction = Above_ | Bottom_ | Left_ | Right_ deriving Eq
type Position = (Int, Int)
type Grid = [[Color]]
type State = (Position, Grid)
type MoveCount = Int

initGrid :: Int -> Int -> Grid
initGrid h w = replicate h $ replicate w White

nextPosition :: Color -> Direction -> Position -> Position -> (Direction, Position)
nextPosition White Above_  (x, y) (_, w) = if y == w then (Right_,  (x, 1)) else (Right_,  (x, y + 1))
nextPosition White Bottom_ (x, y) (_, w) = if y == 1 then (Left_,   (x, w)) else (Left_,   (x, y - 1))
nextPosition White Left_   (x, y) (h, _) = if x == 1 then (Above_,  (h, y)) else (Above_,  (x - 1, y))
nextPosition White Right_  (x, y) (h, _) = if x == h then (Bottom_, (1, y)) else (Bottom_, (x + 1, y))
nextPosition Black Above_  (x, y) (_, w) = if y == 1 then (Left_,   (x, w)) else (Left_,   (x, y - 1))
nextPosition Black Bottom_ (x, y) (_, w) = if y == w then (Right_,  (x, 1)) else (Right_,  (x, y + 1))
nextPosition Black Left_   (x, y) (h, _) = if x == h then (Bottom_, (1, y)) else (Bottom_, (x + 1, y))
nextPosition Black Right_  (x, y) (h, _) = if x == 1 then (Above_,  (h, y)) else (Above_,  (x - 1, y))

showGrid :: Grid -> IO ()
showGrid = mapM_ (BS.putStrLn . foldMap (\c -> if c == White then BS.pack "." else BS.pack "#"))

reverseColor :: Color -> Color
reverseColor White = Black
reverseColor Black = White

move :: Direction -> Position -> MoveCount -> State -> Grid
move _ _ 0 state = snd state
move d (h, w) n ((x, y), grid) = move (fst next) (h, w) (n - 1) (snd next, newGrid)
    where next = nextPosition (grid !! (x - 1) !! (y - 1)) d (x, y) (h, w)
          newGrid = genNewGrid (x, y) grid

genNewGrid :: Position -> Grid -> Grid
genNewGrid _ [] = []
genNewGrid (1, y) (c : cs) = changeRow y c : cs 
genNewGrid (x, y) (c : cs) = c : genNewGrid (x - 1, y) cs

changeRow :: Int -> [Color] -> [Color]
changeRow _ [] = []
changeRow 1 (c : cs) = reverseColor c : cs
changeRow y (c : cs) = c : changeRow (y - 1) cs

main :: IO ()
main = do
    [h, w, n] <- map readInt . BS.words <$> BS.getLine
    let start = (1, 1)
    let grid = initGrid h w
    showGrid $ move Above_ (h, w) n (start, grid)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

