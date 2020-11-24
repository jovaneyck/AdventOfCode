import Data.List.PointedList(PointedList)  -- λ stack install pointedlist
import Data.List.PointedList.Circular as CPL
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromJust)
import Data.List(sort)
-- import Test.HUnit

-- :{

data State = State { nbPlayers :: Int, player :: Int, points :: Map Int Int, circle :: PointedList Int } deriving Show
initialState nbPlayers = State { nbPlayers = nbPlayers,  player = 1, circle = CPL.singleton 0, points = Map.empty }

nextPlayer :: Int -> Int -> Int
nextPlayer max p = 1 + (p `mod` max)

play :: State -> Int -> State
play state marble = 
    if marble `mod` 23 == 0 then
        let next = CPL.moveN (-7) $ circle state
            marble7 = _focus next
            nextCircle = fromJust $ CPL.deleteRight next
            currentPoints = Map.findWithDefault 0 (player state) (points state)
            playerPoints = currentPoints + marble + marble7
        in state { circle = nextCircle 
                 , player = theNextPlayer
                 , points = Map.insert (player state) playerPoints (points state) }
    else
        let next = CPL.insertRight marble $ CPL.moveN 1 $ circle state
        in state { circle = next 
                , player =  theNextPlayer }
    where
        theNextPlayer = nextPlayer (nbPlayers state) (player state)

playAll nbPlayers maxMarble = 
    foldl play (initialState nbPlayers) [1..maxMarble]

solve nbPlayers maxMarble =
    let endState = playAll nbPlayers maxMarble
    in last $ sort $ map snd $ Map.toList $ points endState
    
main = do
    putStrLn $ "Solution: " ++ show (solve 416 7197500)

    -- tests = test [ 
    --     "example" ~: solve 9 25 ~=? 32
    --     , "example" ~: solve 10 1618 ~=? 8317
    --     , "example" ~: solve 13 7999 ~=? 146373
    --     , "example" ~: solve 17 1104 ~=? 2764
    --     , "example" ~: solve 21 6111 ~=? 54718
    --     , "example" ~: solve 30 5807 ~=? 37305
    --     ]

-- :}

-- runTestTT tests