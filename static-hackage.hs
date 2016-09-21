{-# LANGUAGE CPP #-}
-- | Static hackage generator program
--
-- Use it like:
--
-- @
--     static-hackage package1.tar.gz package2.tar.gz ...
-- @
--
-- The above command will generate @00-index.tar.gz@ in current
-- directory and appropriate directory hierarchy in @package@
-- subdirectory. As such this is ready for serving via HTTP. You may
-- use nginx or another static file server for that purpose.
--
-- Files and directories generated:
--
-- ./00-index.tar.gz       -- all .cabal files tared
-- ./$N/$V/$N-$V.cabal     -- same cabal files, static
-- ./package/$N-$V.tar.gz  -- full contents of package
--
-- Where $N is package name and $V is package version.
--
module Main where
import Codec.Archive.Tar (Entries (..), entryPath, entryContent, EntryContent (NormalFile))
import Codec.Compression.GZip (decompress, compress)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Prelude hiding (pi)
import System.Directory
import System.Environment
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Map as Map
import qualified Data.Set as Set

type CabalFile = FilePath
type Tarball = FilePath

type PackageDB = Map PackageName (Set.Set Version)

#if !MIN_VERSION_Cabal(1,22,0)
unPackageName :: PackageName -> String
unPackageName (PackageName s) = s
#endif

tarballName :: PackageName -> Version -> [Char]
tarballName pn v = concat
    [ unPackageName pn
    , "-"
    , showVersion v
    , ".tar.gz"
    ]

cabalName :: PackageName -> Version -> [Char]
cabalName pn v = concat
    [ unPackageName pn
    , "-"
    , showVersion v
    , ".cabal"
    ]

tarballPath :: PackageName -> Version -> IO [Char]
tarballPath pn v = do
    let rd =  "."
    return $ concat
        [ rd
        , '/' : unPackageName pn
        , '/' : showVersion v
        ]


addFileContent :: Map PackageName (Set.Set Version)
               -> L.ByteString
               -> IO (Map PackageName (Set.Set Version))
addFileContent ps content = do
    let entries = Tar.read $ decompress content
    let cabal =
            case getCabal entries of
                NormalFile lbs _ -> lbs
                _ -> error "cabal file must be a NormalFile"
    let di =
            case parsePackageDescription $ U.toString cabal of
                ParseOk _ x -> x
                y -> error $ "Invalid cabal file: " ++ show y
    let pi = package $ packageDescription di
    let name = pkgName pi
    let version = pkgVersion pi
    tp <- tarballPath name version
    let rd = "."
    liftIO $ createDirectoryIfMissing True tp
    liftIO $ L.writeFile (rd ++ "/package/" ++ tarballName name version) content
    liftIO $ L.writeFile (tp ++ '/' : cabalName name version) cabal

    let oldSet = fromMaybe Set.empty $ Map.lookup name ps
    let newSet = Set.insert version oldSet
    let ps' = Map.insert name newSet ps
    return ps'
  where
    getCabal Done = error "No cabal file inside tar"
    getCabal (Next entry rest)
        | reverse (take 6 $ reverse $ entryPath entry) == ".cabal" = entryContent entry
        | otherwise = getCabal rest
    getCabal (Fail s) = error $ "Invalid tarball: " ++ show s

rebuildIndex :: Map PackageName (Set.Set Version) -> IO ()
rebuildIndex ps = do
    let path = "."
    let index = path ++ "/00-index.tar.gz"
    entries <- liftIO $ Tar.pack path $ cabals path
    let entries' = map (fix path) entries
    liftIO $ L.writeFile index $ compress $ Tar.write entries'
  where
    right (Right x) = x
    right (Left x) = error $ "toTarPath returned " ++ x
    fix path entry = entry { Tar.entryTarPath = right $ Tar.toTarPath False $ fix' path $ Tar.fromTarPath $ Tar.entryTarPath entry }
    fix' path fp =
        if take (length path') fp == path'
            then drop (length path') fp
            else fp
      where
        path' = path ++ "/"
    cabals path = concatMap (go path) $ Map.toList ps
    go path (name, vs) = map (go' path name) $ Set.toList vs
    go' path name version = concat
        [ path
        , '/' : unPackageName name
        , '/' : showVersion version
        , '/' : cabalName name version
        ]

main :: IO ()
main = do
    args <- getArgs
    let path = "."
    createDirectoryIfMissing True $ path ++ "/package"
    let addFile ps filename = do
          putStrLn $ "Adding file " ++ filename
          content <- L.readFile filename
          addFileContent ps content
    ps <- foldM addFile Map.empty args
    rebuildIndex ps
