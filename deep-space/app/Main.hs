-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- # LANGUAGE StandaloneDeriving #
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

import System.IO

import SubHask
import SubHask.Algebra.Metric
-- import SubHask.Algebra.Vector
import SubHask.Compatibility.ByteString
-- import SubHask.Category.Trans.Derivative

-- import HLearn.History
-- import HLearn.Optimization.Univariate

-- import System.IO

import qualified Prelude as P
import Control.Monad.Random
import Control.Monad.State
import SubHask.Compatibility.ByteString
import SubHask.Algebra.Container
import SubHask.Algebra.Logic
import SubHask.Algebra
import SubHask.Category
-- import SubHask.Internal.Prelude
import Control.Monad
-- import Control.Monad.Trans.State
import qualified Data.List as L
import System.IO
import HLearn.Data.SpaceTree.CoverTree

--------------------------------------------------------------------------------


-- instance Metric DNA

data BP = A | T | C | G |
    N | Y | S | K | M | W | R | B |
    D | H | V | U deriving (Show)

-- instance Enum BP where
--     {-# INLINE succ #-}
--     succ = P.succ

--     # INLINE toEnum #
--     toEnum i = if i < 0
--         then P.toEnum 0
--         else P.toEnum i

type instance Logic BP = Bool

instance Eq_ BP where
    A==A = True
    T==T = True
    C==C = True
    G==G = True
    N==_ = True
    U==_ = False
    K==x = x==K || x==G || x==T
    S==x = x==S || x==G || x==C
    Y==x = x==Y || x==T || x==C
    M==x = x==M || x==A || x==C
    W==x = x==W || x==A || x==T
    R==x = x==R || x==A || x==G
    B==x = x==B || x==T || x==G || x==C
    D==x = x==D || x==T || x==G || x==A
    H==x = x==H || x==T || x==C || x==A
    V==x = x==V || x==G || x==C || x==A
    _==_ = False

-- instance Enum BP where
--     {-# INLINE succ #-}
--     succ = P.succ

--     {-# INLINE toEnum #-}
--     toEnum i = if i < 0
--         then P.toEnum 0
--         else P.toEnum i

mapToBP :: RandomGen g => (Int, g) -> (BP, g) 
mapToBP (x, ng) = (IntToBP x, ng)

IntToBP :: Int -> BP
IntToBP 0 = A
IntToBP 1 = T
IntToBP 2 = C
IntToBP 3 = G

BPToInt :: BP -> Int
BPToInt A = 0
BPToInt T = 1
BPToInt C = 2
BPToInt G = 3


-- mapFromBP :: (BP, BP) -> StdGen -> (BP, StdGen) 
-- mapFromBP x ng = randomR (fromEnum <$> x) ng

instance Random BP where
    -- randomR r g = randomR r g
    randomR r g = mapToBP $ randomR (BPtoInt P.<$> r) g

    -- NOTE the 15 is hardwired and needs to match BP
    random g = mapToBP $ randomR (0,15) g

data SeqData = SeqData [BP]
    deriving (Show)

randomSeq :: Int -> State StdGen [BP]
randomSeq n = Control.Monad.replicateM n randomBP
    where
        -- only ATCG
        randomBP = state $ randomR (A,G)

-- getSequence :: Int -> StdGen -> ([BP], g)
-- getSequence i gen = (map (\x -> getBP x) randomInts,
--                      g)
--     where
--         randomInt g = randomR (0,3) g
--         randomInts 0 _ = 
--         randomInts n g = ri:(head $
--                              randomInts (n-1) newg)
--             where
--                 (ri, newg)= randomInt g

-- foldDice = Control.Monad.liftM foldl

randomSeqs :: Int -> Int -> State StdGen [[BP]]
randomSeqs len size = Control.Monad.replicateM size (randomSeq len)
 
-- randomSeqs :: Int -> Int -> State (StdGen, [BP]) [BP]
-- randomSeqs 0 size = do
--     (_, bps) <- get
--     Control.Monad.return bps
 

-- randomSeqs len size = do
--     (gen, bps) <- get
--     let (next, g) = runState (randomSeq len) g
--     put (g, next:bps)
--     randomSeqs (len-1) size
 

-- rollDice = Control.Monad.liftM2 (,) rollDie rollDie

-- randomSequence :: Int -> Int -> [[BP]]
-- randomSequence gen i = [[x] | x <- [y] | y <- (rs i)]
--     where
--         getBP x = case x `mod` 4 of
--             0 -> A
--             1 -> T
--             2 -> C
--             3 -> G
--         rs :: [Int] -> [BP]
--         rs n =  map (\x -> getBP x) [1..n]
--         randomInts = P.take i (randoms gen)
--------------------------------------------------------------------------------

main = do
    -- putStrLn $ show (distance a b)
    -- gen <- newStdGen
    -- let randomSequence = randoms gen
    -- gen <- newStdGen
    -- let randomSequence = getSequence $ randoms gen
    -- putStrLn $ show $ randomSequence 10
    -- putStrLn $ show $ randomSequence 10
    -- putStrLn $ show $ randomSequence 10
    -- putStrLn $ show $ randomSequence 10
    -- putStrLn $ show $ A == A
    -- putStrLn $ show $ A == T
    g <- getStdGen
    let a = evalState (randomSeqs 5 10) g
    -- let a = evalState (randomSeqs 5 10) (g,[])
    -- let a = Control.Monad.fmap (rollDie g) [1..3]
    putStrLn $ show $  a
    -- putStrLn $ show $ evalState rollDie (execState a)
    -- putStrLn $ P.take 3 (randomSequence 10)
    -- print . P.take 10 $ (randomRs ('a', 'c') g)
    -- print . P.take 10 $ (randomRs ('a', 'z') g)