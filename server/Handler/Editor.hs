module Handler.Editor where

import Data.Aeson (encode, eitherDecode)
import IdeSession (defaultSessionInitParams, defaultSessionConfig)
import IdeSession.Client (ClientIO(..), startEmptySession)
import IdeSession.Client.CmdLine
import Import
import Yesod.GHCJS
import Yesod.WebSockets

getEditorR :: Handler ()
getEditorR = webSockets $ do
    conn <- ask
    let clientIO = ClientIO
            { putJson = \x -> flip runReaderT conn $ sendTextData $ encode x
            , getJson = flip runReaderT conn $ fmap decodeOrFail receiveData
            }
    liftIO $ startEmptySession clientIO opts EmptyOptions
  where
    -- TODO: fail more gracefully than this, possibly by changing
    -- ide-backend-client to have getJson :: IO (Maybe Value).
    decodeOrFail = either error id . eitherDecode
    opts = Options
        { optInitParams = defaultSessionInitParams
        , optConfig = defaultSessionConfig
        , optCommand = StartEmptySession EmptyOptions
        }

getEditorScriptR :: Handler TypedContent
getEditorScriptR = $(ghcjsFileDev
#if DEVELOPMENT
    True
#else
    False
#endif
    (["-XCPP"
    ,"-XTemplateHaskell"
    ,"-XOverloadedStrings"
    ,"-XViewPatterns"
    ,"-XRankNTypes"
    ,"-XRecordWildCards"
    ,"-XForeignFunctionInterface"
    ,"-XJavaScriptFFI"
    ,"-XLambdaCase"
    ,"-XParallelListComp"
    ,"-XGeneralizedNewtypeDeriving"
    ,"-XScopedTypeVariables"
    ,"-XDeriveDataTypeable"
    ,"-XDeriveGeneric"
    ,"-Wall"
    ,"-hide-all-packages"] ++ concatMap (\pkg -> ["-package", pkg])
        ["aeson"
        ,"async"
        ,"JsonGrammar"
        ,"base"
        ,"bytestring"
        ,"ghcjs-ace"
        ,"ghcjs-base"
        ,"ghcjs-dom"
        ,"ghcjs-jquery"
        ,"ghcjs-prim"
        ,"ghcjs-react"
        ,"ghcjs-websockets"
        ,"ide-backend-common"
        ,"ide-backend-json"
        ,"language-typescript"
        ,"lens"
        ,"mtl"
        ,"stack-prism"
        ,"stm"
        ,"text"
        ,"transformers"
        ,"void"
        ])
    ["client"]
    "client/main.hs")
