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

import Control.Parallel.Strategies
import Control.Parallel

import Data.Char (chr,ord)

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad

import System.Environment (getArgs)

main::IO ()
main = do
  [p,q] <- (map read) <$> getArgs
  unless (p>0 && q>0) $ error "Invalid arguments"
  let chessG = mkChessGraph p q
  let cycles = enumHamCycle chessG
  n <- countAndPrint 0 chessG cycles
  putStrLn $ "Total cycles = "++(show n)
  where
    countAndPrint::Int->Graph->[[Int]]->IO Int
    countAndPrint !accum g [] = return accum
    countAndPrint !accum g (c:cs) = do
      putStrLn $ prettyPath g c
      countAndPrint (accum+1) g cs

data Graph = Graph{
  gNVerts::Int,
  gNames::M.Map Int String,
  gNeighs::M.Map Int [Int]
  } deriving Show

-- Enumerate all Hamiltonian cycles in the given graph
enumHamCycle::Graph->[[Int]]
enumHamCycle g
  = completeHamCycle g 1 (n - 1)
                     [0] (S.singleton 0)
  where
    n = gNVerts g

-- Try to complete a path into a Hamiltonian cycle
--   (parallel version)
completeHamCycle::Graph->Int->Int
                  ->[Int]->(S.Set Int)
                  ->[[Int]]
completeHamCycle g !depth !remain !path !visited
  | remain == 0 =
      if 0 `elem` choices then [path] else []
  | otherwise =
      concat $ withStrategy (evalList strat)
                [completeHamCycle g (depth+1) (remain-1)
                 (c:path)
                 (c `S.insert` visited)
                | c <- choices,
                  c `S.notMember` visited
                ]
  where
    last = head path
    choices = (gNeighs g) M.! last
    strat = if depth>6 then rseq else rpar


-- Make the graph corresponding to
--   a knight's moves on a mxn chessboard
mkChessGraph::Int->Int->Graph   
mkChessGraph m n
  = Graph {gNVerts = nv,
           gNames = M.fromList [(k,genName k)|k<-[0..nv-1]],
           gNeighs = M.fromList [(k,genNeighs k)|k<-[0..nv-1]]
          }
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
prettyPath g  = concat . map ((gNames g) M.!) 


