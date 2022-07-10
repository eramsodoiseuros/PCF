{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad
import Data.List

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1  = 1
getTimeAdv P2  = 2
getTimeAdv P5  = 5
getTimeAdv P10 = 10

-- Utilities
-- lanterna
lamp :: Objects
lamp = Right ()

-- mover a lanterna
moveLamp :: State -> State
moveLamp = changeState lamp

-- lista dos aventureiros como objetos
advo :: [Objects]
advo = map Left [P1, P2, P5, P10]

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]

-- lista dos aventureiros como objetos que podem atravessar a ponte
-- (estão no mesmo lado da lanterna)
canCross :: State -> [Objects]
canCross s = filter ((== s lamp) . s) advo

-- igual a getTimeAdv só que para os objetos
getTimeOAdv :: Objects -> Int
getTimeOAdv (Left a) = getTimeAdv a

-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice [ moveOne s, moveTwo s ]

moveOne :: State -> ListDur State
moveOne s = LD (map f lo) 
               where
                  -- aventureiros do mesmo lado que a lanterna
                  -- lo :: [Objects]
                  lo  = canCross s
                  -- estado após aventureiro e lanterna atravessarem a ponte
                  -- f :: State -> Duration State
                  f x = wait (getTimeOAdv x) (return $ changeState x $ moveLamp s)

-- retorna os pares possiveis de aventurerios dados
pairs :: [Objects] -> [(Objects, Objects)]
pairs [] = []
pairs (x:xs) = (map (\i -> (x, i)) xs) ++ (pairs xs)

moveTwo :: State -> ListDur State
moveTwo s = LD (map f po)
               where 
                  -- pares dos aventureiros que podem atravessar a ponte
                  -- po :: [(Objects, Objects)]
                  po = pairs $ canCross s
                  -- durações dos pares após atravessarem
                  -- f :: (Objects, Objects) -> State
                  f (x, y) = wait (m x y) (c x y)
                  -- estado após dois aventureiros e lanterna atravessarem
                  -- c :: Objects -> Objects -> m State
                  c x y = return $ changeState x $ changeState y $ moveLamp s
                  -- tempo que demoram os dois aventueiros a atravessar
                  -- m :: Objects -> Objects -> Int
                  m x y = max (getTimeOAdv x) (getTimeOAdv y)

{--
moveTwo :: State -> ListDur State
moveTwo s = manyChoice (map (moveTwo' lo s) lo)
               where
                  --  aventureiros do mesmo lado que a lanterna
                  -- lo :: [Objects]
                  lo = canCross s

-- inefeciente porque repete pares
moveTwo' :: [Objects] -> State -> Objects -> ListDur State
moveTwo' lo s o = LD (map f lo')
                     where
                        -- duração dos dois aventureiros atravessarem a ponte
                        -- f :: Objects -> Duration State
                        f x  = wait (m x) (s' x)
                        -- tempo que demoram os aventueiros a atravessar
                        -- m :: Objects -> Int
                        m x  = max (getTimeOAdv o) (getTimeOAdv x)
                        -- estado após o par de aventureiros e laterna atravessarem
                        -- s' :: Objects -> m State
                        s' x = return (changeState x (changeState o (moveLamp s)))
                        -- lista dos objectos diferentes do aventureiro atual
                        -- lo' :: [Objects]
                        lo'  = filter (/= o) lo
--}

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s' <- allValidPlays s
              exec (n-1) s'

exec5 s = do s1 <- allValidPlays s
             s2 <- allValidPlays s1
             s3 <- allValidPlays s2
             s4 <- allValidPlays s3
             s5 <- allValidPlays s4
             return s5


{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = any (\(Duration (x, s)) -> x<=17 && all s advo) (remLD (exec 5 gInit))

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = any id [any f (remLD (exec i gInit)) | i <- [1..8]]
         where
            -- duração menor que 17 e todos atravessaram
            -- f :: Duration State -> Bool
            f (Duration (x, s)) = x<17 && all s advo


--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement
instance Functor ListDur where
   fmap f = LD . (map (fmap f)) . remLD

-- To implement
instance Applicative ListDur where
   pure x = LD [pure x]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ x <*> y

-- To implement
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where 
                        g x = map ((x >>=) . const) (remLD (k $ getValue x))


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------


data LogDur a = LogD [(String, Duration a)]

remLogD :: LogDur a -> [(String, Duration a)]
remLogD (LogD x) = x

instance Functor LogDur where
   fmap f = LogD . (map (\(x, y) -> (x, fmap f y))) . remLogD

instance Applicative LogDur where
   pure x = LogD [([], pure x)]
   l1 <*> l2 = LogD $ do x <- remLogD l1
                         y <- remLogD l2
                         g(x,y) where
                           g((s,f),(s',x)) = return (s ++ s', f <*> x)


instance Monad LogDur where
   return = pure
   l >>= k = LogD $ do x <- remLogD l
                       g x where
                           g (s,x) = map (\(s',x') -> (s ++ s', ((x >>=) . const) x')) (remLogD (k $ getValue x))


manyLChoice :: [LogDur a] -> LogDur a
manyLChoice = LogD . concat . (map remLogD)

allValidLPlays :: State -> LogDur State
allValidLPlays s = manyLChoice [ moveLOne s, moveLTwo s ]

moveLOne :: State -> LogDur State
moveLOne s = manyLChoice (map f lo) 
               where
                  lo  = canCross s
                  f x = mwrite (" "++(show x)++" ") $ mwait (getTimeOAdv x) (return $ changeState x $ moveLamp s)

moveLTwo :: State -> LogDur State
moveLTwo s = manyLChoice (map f po)
               where 
                  po = pairs $ canCross s
                  f (x, y) = mwrite (" "++(show (x, y))++" ") $ mwait (m x y) (c x y)
                  c x y = return $ changeState x $ changeState y $ moveLamp s
                  m x y = max (getTimeOAdv x) (getTimeOAdv y)

mwrite :: String -> LogDur a -> LogDur a
mwrite msg =  LogD . map (\(s,x) -> (s ++ msg, x)) . remLogD

mwait :: Int -> LogDur a -> LogDur a
mwait i = LogD . map (\(x, y) -> (x, wait i y)) . remLogD

leqL17 :: Maybe (String, Duration State)
leqL17 = find (\(_, Duration (x, s)) -> x<=17 && all s advo) $ remLogD $ execL 5 gInit

execL :: Int -> State -> LogDur State
execL 0 s = return s
execL n s = do s' <- allValidLPlays s
               execL (n-1) s'