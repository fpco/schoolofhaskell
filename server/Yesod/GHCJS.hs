{-# LANGUAGE OverloadedStrings #-}

-- | GHCJS support for Yesod.

module Yesod.GHCJS
    (ghcjsFileDev)
    where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Combinators as CL
import           Data.Conduit.Process
import           Data.FileEmbed (embedFile)
import           Data.Maybe
import           Data.String (fromString)
import           Filesystem.Path (hasExtension)
import qualified Filesystem.Path.CurrentOS as FP
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Temp
import           Yesod.Core

-- | Make a widget by compiling the module to JS to a temporary file
-- and then loading in the JS. Results in a @Handler TypedContent@
-- expression.
ghcjsFileDev :: Bool -> [String] -> [FilePath] -> FilePath -> Q Exp
ghcjsFileDev development args folders fp = do
    let args' = ["-i"] ++ map (\folder -> "-i" ++ folder) folders ++ args
    if development
        then [| liftIO $ do
            genfp <- runGhcjs fp args'
            content <- BS.readFile genfp
            return $ TypedContent typeJavascript $ toContent content |]
        else do
            genfp <- runIO $ runGhcjs fp args'
            sourceFiles <- runIO $ runResourceT $
              mapM_ (CL.sourceDirectoryDeep False . fromString) folders $=
              CL.filter (`hasExtension` "hs") $$
              CL.sinkList
            mapM_ (qAddDependentFile . FP.encodeString) sourceFiles
            bs <- embedFile genfp
            f <- [|return . TypedContent typeJavascript . toContent|]
            return $ f `AppE` bs

runGhcjs :: FilePath -> [String] -> IO FilePath
runGhcjs fp args = do
  let dir = "dist/ghcjs-cache/"
      genfp = dir ++ slugize fp ++ ".js"
  createDirectoryIfMissing True dir
  runGhcJSAll args fp $ \f -> do
    putStrLn ("Copying " ++ f ++ " to " ++ genfp ++ " ...")
    copyFile f genfp
  return genfp

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
          [fp,"-outputdir","dist/ghcjs","-o",tmpdir] ++
          args
