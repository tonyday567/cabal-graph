{-# LANGUAGE OverloadedStrings #-}

-- | Ecosystem mapper for ~/haskell/.
module Main where

import Algebra.Graph
import CabalFix
import CabalGraph (libDeps)
import Control.Monad (forM)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DotParse qualified as Dot
import Optics
import System.Directory
import System.FilePath

-- | Packages to hide from the diagram (benchmark/meta/isolated).
hiddenPkgs :: Set.Set C.ByteString
hiddenPkgs =
  Set.fromList
    [ "perf",
      "mnet",
      "sysl",
      "grepl",
      "agent-fork",
      "circuits-llm",
      "hcount",
      "memo"
    ]

main :: IO ()
main = do
  home <- getHomeDirectory
  let root = home </> "haskell"
  cabals <- findCabalFiles root
  putStrLn $ "Found " ++ show (length cabals) ++ " cabal files"

  parsed <- mapM parseCabalFile cabals
  let pkgs0 = Map.fromList [(name, deps) | Just (name, deps) <- parsed]
      -- hide selected packages from diagram, and specific edges
      pkgs1 = Map.map (filter (not . (`Set.member` hiddenPkgs))) pkgs0
      pkgs = Map.mapWithKey (\name deps -> if name == "markup-parse" then filter (/= "circuits-parser") deps else deps) pkgs1
      localNames = Set.difference (Map.keysSet pkgs) hiddenPkgs
      n = Set.size localNames
  putStrLn $ "Diagramming " ++ show n ++ " packages"

  -- local interdependency graph (filtered)
  let localEdges = [(name, filter (`Set.member` localNames) deps) | (name, deps) <- Map.toList pkgs, not (name `Set.member` hiddenPkgs)]
      localGraph = stars localEdges
  writeDot "local-eco.dot" localGraph
  putStrLn "Wrote local-eco.dot"

  -- upstream graph (filtered)
  let upstreamEdges = [(name, filter (not . (`Set.member` localNames)) deps) | (name, deps) <- Map.toList pkgs, not (name `Set.member` hiddenPkgs)]
      upstreamGraph = stars upstreamEdges
  writeDot "upstream-eco.dot" upstreamGraph
  putStrLn "Wrote upstream-eco.dot"

  -- common upstreams
  let allUpstream = concatMap snd upstreamEdges
      counts = Map.fromListWith (+) [(dep, 1 :: Int) | dep <- allUpstream]
      commonUpstream = Map.filter (>= 2) counts
      commonNames = Map.keysSet commonUpstream
      commonEdges = [(name, filter (`Set.member` commonNames) deps) | (name, deps) <- Map.toList pkgs, not (name `Set.member` hiddenPkgs)]
      commonGraph = stars commonEdges
  writeDot "common-upstream-eco.dot" commonGraph
  putStrLn "Wrote common-upstream-eco.dot"

  putStrLn "\nTop upstream dependencies:"
  let top = List.take 30 $ List.sortOn (negate . snd) $ Map.toList counts
  mapM_ (\(name', c) -> putStrLn $ "  " ++ C.unpack name' ++ ": " ++ show c) top

  putStrLn "\nLeaves (no local dependents):"
  let leaves = Set.toList $ localNames Set.\\ Set.fromList (concatMap snd localEdges)
  mapM_ (putStrLn . ("  " ++) . C.unpack) leaves

  putStrLn "\nRoots (no local dependencies):"
  let roots = [name | (name, deps) <- localEdges, null deps]
  mapM_ (putStrLn . ("  " ++) . C.unpack) roots

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles root = do
  entries <- listDirectory root
  fmap concat $ forM entries $ \entry -> do
    let path = root </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then do
        files <- listDirectory path
        return [path </> f | f <- files, takeExtension f == ".cabal"]
      else return []

parseCabalFile :: FilePath -> IO (Maybe (C.ByteString, [C.ByteString]))
parseCabalFile fp = do
  bs <- B.readFile fp
  case parseCabalFields defaultConfig bs of
    Left err -> do
      putStrLn $ "  skip " ++ fp ++ ": " ++ C.unpack err
      return Nothing
    Right cf -> do
      let name = pname cf
          deps = fmap dep (libDeps cf)
      return $ Just (name, deps)

writeDot :: FilePath -> Graph C.ByteString -> IO ()
writeDot fp g = do
  let baseGraph =
        Dot.defaultGraph
          & Dot.attL Dot.GraphType (Dot.ID "size")
          .~ Just (Dot.IDQuoted "10!")
          & Dot.attL Dot.GraphType (Dot.ID "ranksep")
          .~ Just (Dot.ID "2.5")
          & Dot.attL Dot.GraphType (Dot.ID "nodesep")
          .~ Just (Dot.ID "0.8")
          & Dot.attL Dot.NodeType (Dot.ID "shape")
          .~ Just (Dot.ID "box")
          & Dot.attL Dot.NodeType (Dot.ID "fontsize")
          .~ Just (Dot.ID "14")
          & Dot.attL Dot.NodeType (Dot.ID "height")
          .~ Just (Dot.ID "0.8")
          & Dot.gattL (Dot.ID "rankdir")
          .~ Just (Dot.IDQuoted "BT")
      g' = Dot.toDotGraphWith Dot.Directed baseGraph g
      txt = Dot.dotPrint Dot.defaultDotConfig g'
  B.writeFile fp txt
