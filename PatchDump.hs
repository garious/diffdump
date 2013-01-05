import System.Process
import System.Environment
import System.Directory
import System.IO
import Control.Exception

main :: IO ()
main = do
    withTempFile "patchdump" patch

patch :: FilePath -> Handle -> IO ()
patch tmpFile hdl = do
    cnts <- getContents
    hPutStr hdl cnts
    hClose hdl

    args <- getArgs
    _ <- readProcess "patch" (args ++ [tmpFile]) ""
    cnts' <- readFile tmpFile
    putStr cnts'

{- Borrowed from Real World Haskell -}

{- This function takes two parameters: a filename pattern and another
   function.  It will create a temporary file, and pass the name and Handle
   of that file to the given function.

   The temporary file is created with openTempFile.  The directory is the one
   indicated by getTemporaryDirectory, or, if the system has no notion of
   a temporary directory, "." is used.  The given pattern is passed to
   openTempFile.

   After the given function terminates, even if it terminates due to an
   exception, the Handle is closed and the file is deleted. -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do -- The library ref says that getTemporaryDirectory may raise on
       -- exception on systems that have no notion of a temporary directory.
       -- So, we run getTemporaryDirectory under catch.  catch takes
       -- two functions: one to run, and a different one to run if the
       -- first raised an exception.  If getTemporaryDirectory raised an
       -- exception, just use "." (the current working directory).
       tempdir <- getTemporaryDirectory `onException` return "."
       (tempfile, temph) <- openTempFile tempdir pattern 

       -- Call (func tempfile temph) to perform the action on the temporary
       -- file.  finally takes two actions.  The first is the action to run.
       -- The second is an action to run after the first, regardless of
       -- whether the first action raised an exception.  This way, we ensure
       -- the temporary file is always deleted.  The return value from finally
       -- is the first action's return value.
       finally (func tempfile temph) 
               (do hClose temph
                   removeFile tempfile)
