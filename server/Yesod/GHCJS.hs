-- | GHCJS support for Yesod.

module Yesod.GHCJS
    (ghcjsFileDev)
    where

import Data.Char
import Data.Conduit.Process
import Data.FileEmbed (embedFile)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude
-- import Settings
import System.Directory
import System.Environment
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Temp
import Yesod.Core

-- | Make a widget by compiling the module to JS to a temporary file
-- and then loading in the JS. Results in a @Handler TypedContent@
-- expression.
ghcjsFileDev :: Bool -> [String] -> FilePath -> Q Exp
ghcjsFileDev development args fp =
  do qAddDependentFile fp -- FIXME really need deep dependency tracking
     let dir = "dist/ghcjs-cache/"
         genfp = dir ++ slugize fp ++ ".js"
     runIO (do createDirectoryIfMissing True dir
               runGhcJSAll
                 args
                 fp
                 (\f ->
                    do putStrLn ("Copying " ++ f ++ " to " ++ genfp ++ " ...")
                       copyFile f genfp))
     if development
         then [|sendFile typeJavascript genfp|]
         else do
            bs <- embedFile genfp
            f <- [|return . TypedContent typeJavascript . toContent|]
            return $ f `AppE` bs

-- | Make a slug from a file path.
slugize :: FilePath -> String
slugize = map replace
  where replace c
          | isLetter c || isDigit c = toLower c
          | otherwise = '_'

-- | Run ghcjs on a file and return the all.js content.
runGhcJSAll :: [String] -> FilePath -> (FilePath -> IO a) -> IO a
runGhcJSAll args fp cont =
  do tempDir <- getTemporaryDirectory
     withTempDirectory
       tempDir
       "run-ghcjs."
       (\tmpdir ->
          do path <- fmap (fromMaybe "ghcjs") (lookupEnv "GHCJS_PATH")
             (Just inh,Nothing,Nothing,p) <- createProcess (config path tmpdir)
             hClose inh
             let display = path ++ " " ++
                           unwords (mkArgs tmpdir)
             putStrLn display
             code <- waitForProcess p
             case code of
               ExitSuccess ->
                  cont (tmpdir ++ "/all.js")
               ExitFailure e -> error ("The GHCJS process failed with exit code " ++ show e ++ "."))
  where config path tmpdir =
          (proc path (mkArgs tmpdir)) {close_fds = True
                                             ,std_in = CreatePipe}
        mkArgs tmpdir =
          [fp,"-o",tmpdir] ++
          args
