{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import System.Console.CmdArgs
  ( cmdArgs
  , args -- remaining arguments
  , opt  -- optional
  , (&=)
  , def
  , typ
  , typFile
  , typDir
  , argPos
  , summary
  , help
  , name
  , Data
  , Typeable
  )
import System.FilePath
  ( (</>)
  , takeDirectory
  , dropTrailingPathSeparator
  )
import System.FilePath.Find  -- From the 'filemanip' package
  ( find
  , extension
  , (==?)
  , (&&?)
  , (||?)
  , FilterPredicate
  , filePath
  , fileName
  )
import System.Process.ByteString
  ( readProcessWithExitCode
  )
import System.Directory
  ( createDirectoryIfMissing
  , createDirectory
  , doesFileExist
  , doesDirectoryExist
  , removeDirectoryRecursive
  , getTemporaryDirectory
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Exit
  ( ExitCode(ExitSuccess)
  , exitFailure
  )
import System.IO
  ( stderr
  , hPutStrLn
  )
import System.IO.Unsafe
  ( unsafePerformIO
  )
import GHC.Conc
  ( getNumProcessors
  )
import Data.List
  ( stripPrefix
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Monad
  ( filterM
  , when
  , unless
  )
import Control.Concurrent.ParallelIO.Local
  ( parallel_
  , withPool
  , Pool
  )
import Control.Exception
  ( finally
  , bracket
  , throwIO
  )
import System.IO.Error
  ( isAlreadyExistsError
  )
import System.Posix.IO
  ( stdOutput
  )
import System.Posix.Terminal
  ( queryTerminal
  )
import System.Console.ANSI
  ( SGR(SetColor, Reset)
  , ConsoleLayer(Foreground)
  , ColorIntensity(Dull)
  , Color(Red, Cyan, Green)
  , setSGR
  )
import Paths_diffdump
  ( version
  )
import Data.Version
  ( showVersion
  )
import System.IO.Error
  ( catchIOError  -- Use this instead of 'catch' to support GHC 7.6.1 and 7.4.x
  )

data DiffDump = DiffDump {
    outfile       :: FilePath
  , outdir        :: DirPath
  , olddir        :: DirPath
  , newdir        :: DirPath
  , func          :: FilePath
  , arg_list      :: [String]
  , cmd           :: FilePath
  , arg           :: [String]
  , cmd1          :: FilePath
  , arg1          :: [String]
  , cmd2          :: FilePath
  , arg2          :: [String]
  , filetype      :: String
  , fileext       :: String
  , diff          :: FilePath
  , diff_arg      :: [String]
  , force         :: Bool
  , ignore        :: [String]
  , recursive     :: Bool
  , unified       :: Bool
  , color         :: Bool
  , no_color      :: Bool
  , verbose       :: Bool
  , keep_files    :: Bool
  , succeed_on_diff :: Bool
} deriving (Show, Data, Typeable)

type DirPath = FilePath

diffDumpOpts :: DirPath -> DiffDump
diffDumpOpts defOutdir = DiffDump {
   -----------------------------------------------------------------------
   -- name          default                type             description --
   -----------------------------------------------------------------------
     olddir       = def    &= argPos 0  &= typ "OLD_DIR" &= opt ("-" :: FilePath)
   , newdir       = def    &= argPos 1  &= typ "NEW_DIR" &= opt ("" :: FilePath)
   , func         = def    &= argPos 2  &= typFile       &= opt ("" :: FilePath)
   , arg_list     = def    &= args      &= typ "ARG"
   , outfile      = def    &= name "o"  &= typFile       &= help "Output path."
   , outdir       = defOutdir           &= typDir        &= help "Directory for tempory files."
   , diff         = "diff"              &= typFile       &= help "Path to diff."
   , cmd          = def                 &= typFile       &= help "Command to run on both arguments."
   , arg          = def                 &= typ "ARG"     &= help "Argument to pass to both commands."
   , cmd1         = def                 &= typFile       &= help "Command to run on first argument.  Overrides --cmd"
   , cmd2         = def                 &= typFile       &= help "Command to run on second argument.  Overrides --cmd"
   , arg1         = def                 &= typ "ARG"     &= help "Argument appended to first command."
   , arg2         = def                 &= typ "ARG"     &= help "Argument appended to second command."
   , filetype     = def                                  &= help "Filter by files that include this string in the output of the 'file' command."
   , fileext      = def                                  &= help "Filter by files that have this file extension."
   , ignore       = def                 &= typDir        &= help "Ignore contents of the given directory."
   , diff_arg     = def                                  &= help "Pass <arg> on to diff."
   , force        = def    &= name "f"                   &= help "Run despite the output directories already existing."
   , recursive    = def    &= name "r"                   &= help "Recursively compare any subdirectories found."
   , unified      = def    &= name "u"                   &= help "Output 3 lines of unified context."
   , color        = def                                  &= help "Force colorized diff in terminal output."
   , no_color     = def                                  &= help "Do not colorize diff in terminal output."
   , verbose      = def                                  &= help "Verbose output."
   , keep_files   = def                                  &= help "Keep intermediary files."
   , succeed_on_diff = def                               &= help "Unlike diff, return zero exit code when there are differences."
   } &= summary ("diffdump " ++ showVersion version)

data DumpCfg = DumpCfg {
    dumpPath        :: FilePath
  , dumpArgs        :: [String]
  } deriving (Show, Eq)

data DiffCfg = DiffCfg {
    diffPath :: FilePath
  , diffArgs :: [String]
  } deriving (Show, Eq)

main :: IO ()
main = do
    tmpDir <- getTemporaryDirectory
    defaultOutputDir <- createUniqueDirectory 0 (tmpDir </> ".diffdump")

    as <- cmdArgs (diffDumpOpts defaultOutputDir)

    n <- getNumProcessors
    when (verbose as) $ do
      hPutStrLn stderr $ "diffdump: Utilizing " ++ show n ++ " CPU(s)."

    finally (withPool n (parallelMain as)) $
        removeDirectoryRecursive defaultOutputDir

parallelMain :: DiffDump -> Pool -> IO ()
parallelMain as threadPool = do
    let oldRoot   = dropTrailingPathSeparator (olddir as)
    let newRoot   = if null (newdir as) then oldRoot else dropTrailingPathSeparator (newdir as)
    let outputDir = dropTrailingPathSeparator (outdir as)

    let firstCfg  = mkDumpCfg (cmd1 as) (func as ++ cmd as) (arg_list as ++ arg as ++ arg1 as)
    let secondCfg = mkDumpCfg (cmd2 as) (func as ++ cmd as) (arg_list as ++ arg as ++ arg2 as)

    let recArgs   = if recursive as then ["-r"] else []
    let uniArgs   = if   unified as then ["-u"] else []
    let diffCfg   = DiffCfg (diff as) (recArgs ++ uniArgs ++ diff_arg as)

    let aDir = outputDir </> "a"
    let bDir = outputDir </> "b"

    createTempDirectory (force as) aDir
    createTempDirectory (force as) bDir

    oldRootIsDir <- doesDirectoryExist oldRoot
    newRootIsDir <- doesDirectoryExist newRoot
    
    let diffdump = do
        if not oldRootIsDir || not newRootIsDir
           then do
             when (recursive as) $ do
                if not oldRootIsDir
                  then hPutStrLn stderr $ "diffdump: error: " ++ oldRoot ++ " is not a directory"
                  else hPutStrLn stderr $ "diffdump: error: " ++ newRoot ++ " is not a directory"
                exitFailure
                
             dumpObjFiles aDir bDir (firstCfg, secondCfg) oldRoot newRoot
           else do
             when (verbose as) $ do
               hPutStrLn stderr $ "diffdump: Scanning for files..."

             oldPaths    <- getRelPaths (recursive as) (filetype as) (fileext as) (ignore as) oldRoot
             commonPaths <- filterM (doesFileExist . (newRoot </>)) oldPaths
    
             when (verbose as) $ do
               hPutStrLn stderr $ "diffdump: Found " ++ show (length commonPaths) ++ " file pairs to compare."
               hPutStrLn stderr $ "diffdump: Generating dump files... "
    
             let dumpOps = map (dumpObj aDir bDir (firstCfg, secondCfg) oldRoot newRoot) commonPaths
             parallel_ threadPool dumpOps

        when (verbose as) $ do
          hPutStrLn stderr $ "diffdump: Comparing files... "

        runDiff diffCfg outputDir "a" "b"

    diffMaybe <- finally diffdump $
        unless (keep_files as) $ do
            removeDirectoryRecursive aDir
            removeDirectoryRecursive bDir

    case diffMaybe of
       Nothing -> return ()
       Just s  -> do
                    isatty <- queryTerminal stdOutput
                    let isColor = not (no_color as) && (color as || isatty)
                    writeDiff isColor (unified as) (outfile as) s

                    when (verbose as) $ do
                      hPutStrLn stderr $ "diffdump: Done."
    
                    unless (succeed_on_diff as) exitFailure

mkDumpCfg :: String -> String -> [String] -> DumpCfg
mkDumpCfg "" "" = DumpCfg "cat"  -- Default command, plus custom options
mkDumpCfg ""  x = DumpCfg x      -- --cmd as command, plus custom options
mkDumpCfg  x  _ = DumpCfg x      -- Override all defaults

createTempDirectory :: Bool -> DirPath -> IO ()
createTempDirectory frce p = do
    pExists <- doesDirectoryExist p
    when (pExists && not frce) $ do
        hPutStrLn stderr $ "error: the temporary directory '" ++ p ++ "' already exists.  Use '-f' to proceed anyway."
        exitFailure
    createDirectoryIfMissing True p

writeDiff :: Bool -> Bool -> FilePath -> B.ByteString -> IO ()
writeDiff True   isUni "" = putColorizedDiff isUni
writeDiff False _isUni "" = B.putStr
writeDiff _color _isUni p = B.writeFile p

putColorizedDiff :: Bool -> B.ByteString -> IO ()
putColorizedDiff isUni s = do
    mapM_ (putColorizedLine isUni) (B8.lines s)
    setSGR [Reset]  -- cleanup

putColorizedLine :: Bool -> B.ByteString -> IO ()
putColorizedLine True x  = setSGR [getUnifiedColor (safeHead ' ' x)] >> B8.putStrLn x
putColorizedLine False x = setSGR [       getColor (safeHead ' ' x)] >> B8.putStrLn x

safeHead :: Char -> B.ByteString -> Char
safeHead x xs
  | B.null xs = x
  | otherwise = B8.head xs

getColor :: Char -> SGR
getColor '>' = SetColor Foreground Dull Green
getColor '<' = SetColor Foreground Dull Red
getColor  _  = Reset

getUnifiedColor :: Char -> SGR
getUnifiedColor '@' = SetColor Foreground Dull Cyan
getUnifiedColor '+' = SetColor Foreground Dull Green
getUnifiedColor '-' = SetColor Foreground Dull Red
getUnifiedColor  _  = Reset

dumpObj :: DirPath -> DirPath -> (DumpCfg, DumpCfg) -> DirPath -> DirPath -> FilePath -> IO ()
dumpObj aDir bDir (cfg1, cfg2) oldRoot newRoot p = do
    runDump cfg1 (aDir</>p) (oldRoot</>p) ""
    runDump cfg2 (bDir</>p) (newRoot</>p) ""

dumpObjFiles :: DirPath -> DirPath -> (DumpCfg, DumpCfg) -> FilePath -> FilePath -> IO ()
dumpObjFiles aDir bDir (cfg1, cfg2) oldPath newPath = do
    input <- if oldPath == "-" || newPath == "-" then B.getContents else return ""
    runDump cfg1 (aDir</>"dump.txt") oldPath input
    runDump cfg2 (bDir</>"dump.txt") newPath input

runDiff :: DiffCfg -> DirPath -> FilePath -> FilePath -> IO (Maybe B.ByteString)
runDiff cfg outputDir p1 p2 = do
   bracket getCurrentDirectory setCurrentDirectory $ const $ do
       setCurrentDirectory outputDir
       (exitCode, s, err) <- readProcessWithExitCode (diffPath cfg) (diffArgs cfg ++ [p1, p2]) ""
       B.hPutStr stderr err
       return $ case exitCode of
          ExitSuccess -> Nothing
          _           -> Just s

createUniqueDirectory :: Int -> FilePath -> IO FilePath
createUniqueDirectory n p = do
      (createDirectory tmp >> return tmp)
         `catchIOError` (\e -> if isAlreadyExistsError e
                               then createUniqueDirectory (succ n) p
                               else throwIO e)
  where
    tmp = p ++ show n

runDump :: DumpCfg -> FilePath -> FilePath -> B.ByteString -> IO ()
runDump cfg outPath p input = do
    let as = dumpArgs cfg ++ (if p == "-" then [] else [p])
    s <- readProcess (dumpPath cfg) as input
    createDirectoryIfMissing True (takeDirectory outPath)
    B.writeFile outPath s

readProcess :: FilePath -> [String] -> B.ByteString -> IO B.ByteString
readProcess nm as input = do
    (exitCode, s, err) <- readProcessWithExitCode nm as input
    case exitCode of
      ExitSuccess -> return s
      _ -> do
        hPutStrLn stderr $ "diffdump: error executing: " ++ unwords (nm : as)
        B8.hPutStrLn stderr err
        exitFailure

getRelPaths :: Bool -> String -> String -> [String] -> DirPath -> IO [FilePath]
getRelPaths isRecursive fileTy fileExt ignoreDirs p = do
    ps <- find ((return isRecursive &&? isGoodDir) ||? filePath ==? p) ((return (null fileExt) ||? extension ==? fileExt) &&? expectedFileType fileTy) p
    return $ map (stripRoot p) ps
  where
    isGoodDir = do
        nm <- fileName
        return $ nm `notElem` ignoreDirs

expectedFileType :: String -> FilterPredicate
expectedFileType "" = return True
expectedFileType s  = do
    p <- filePath
    return $ B8.pack s `B8.isInfixOf` unsafePerformIO (readProcess "file" ["--brief", p] "")

stripRoot :: DirPath -> FilePath -> FilePath
stripRoot pre p = maybe p id (stripPrefix (pre ++ "/") p)

