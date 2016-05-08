{-#LANGUAGE BangPatterns #-}

{-
Enumerate all closed Knight's Tours in an
arbitrarity sized chessboard. Multi-threaded version.

Usage:
    KnightTours m n +RTS -N

Compile with: 
    ghc -O3 -threaded -rtsopts -fllvm --make KnightTours

Examples:
    A small example is the 3x12 board

Author: Jyotirmoy Bhattacharya (jyotirmoy@jyotirmoy.net)

This program has been put into the public domain
by the author.
-}

module Main (main) where

import Prelude hiding ((!),read,replicate)
import qualified Prelude

import Control.Parallel.Strategies
import Control.Parallel

import Data.Char (chr,ord)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Generic ((!),(//),
                            toList,generate,
                            replicate,freeze,thaw)
import Data.Vector.Generic.Mutable (read,write)

import Control.Monad.ST
import Control.Monad

import System.Environment (getArgs)

main::IO ()
main = do
  [p,q] <- (map Prelude.read) <$> getArgs
  guard (p>0 && q>0)
  let chessG = mkChessGraph p q
  let cycles = enumHamCycle chessG
  n <- countAndPrint 0 chessG cycles
  putStrLn $ "Total cycles = "++(show n)
  where
    countAndPrint::Int->Graph->[V.Vector Int]->IO Int
    countAndPrint !accum g [] = return accum
    countAndPrint !accum g (c:cs) = do
      putStrLn $ prettyPath g (toList c)
      countAndPrint (accum+1) g cs

data Graph = Graph{
  gNVerts::Int,
  gNames::BV.Vector String,
  gNeighs::BV.Vector [Int]
  } deriving Show

-- Enumerate all Hamiltonian cycles in the given graph
enumHamCycle::Graph->[V.Vector Int]
enumHamCycle g
  = completeHamCyclePar g 1 (n - 1)
                        path visited
  where
    n = gNVerts g
    path = replicate n 0
    visited = (replicate n False) // [(0,True)]

-- Try to complete a path into a Hamiltonian cycle
--   (parallel version)
completeHamCyclePar::Graph->Int->Int
                     ->V.Vector Int->V.Vector Bool
                     ->[V.Vector Int]
completeHamCyclePar g !depth !remain !path !visited
  | remain == 0 =
      if 0 `elem` choices then [path] else []
  | depth < 6 =
      concat $ withStrategy (evalList rpar)
                [completeHamCyclePar g (depth+1) (remain-1)
                 (path // [(depth,c)])
                 (visited // [(c,True)])
                | c <- choices,
                  not $ visited ! c
                ]
  | otherwise =
      runST $ do
        p <- thaw path
        v <- thaw visited
        completeHamCycleSeq g depth remain p v
  where
    last=  path ! (depth-1)
    choices = (gNeighs g) ! last

-- Try to complete a path into a Hamiltonian cycles
--   (sequential version)
completeHamCycleSeq::Graph->Int->Int
                   ->MV.MVector s Int->MV.MVector s Bool
                   ->ST s [V.Vector Int]
completeHamCycleSeq g !depth !remain !path !visited = do
  last <- read path (depth-1)
  let !choices = (gNeighs g) ! last
  if remain == 0 then
    if 0 `elem` choices then
      do
        ans <- freeze path
        return [ans]
    else
      return []
  else
   do
      validChoices <- filterM (\c-> not <$> read visited c) choices
      children <- forM validChoices $ \c -> do
        write path depth c
        write visited c True
        ans <- completeHamCycleSeq g (depth+1) (remain-1)
          path visited
        write visited c False
        ans `pseq` (return ans)
      return $ concat children

-- Make the graph corresponding to
--   a knight's moves on a mxn chessboard
mkChessGraph::Int->Int->Graph   
mkChessGraph m n
  = Graph {gNVerts = nv,
           gNames = generate nv genName,
           gNeighs = generate nv genNeighs}
  where
    nv = m*n
    deidx k = k `divMod` n
    idx i j = i*n+j
    genName k = let (i,j) = deidx k in
      chr (ord 'A'+i):(show j)
    genNeighs k = let (i,j) = deidx k in
      [idx p q|
       (x,y)<-[(1,2),(1,-2),(-1,2),(-1,-2),
               (2,1),(2,-1),(-2,1),(-2,-1)],
       let p = i+x,
       let q = j+y,
       p>=0 && p<m && q>=0 && q<n]

-- Pretty print path in a graph
prettyPath::Graph->[Int]->String
prettyPath g  = concat . map ((gNames g) !) 


