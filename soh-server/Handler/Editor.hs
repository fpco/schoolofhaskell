module Handler.Editor where

import Import
import Yesod.GHCJS

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
    ,"-XStandaloneDeriving"
    ,"-XTypeFamilies"
    ,"-XFlexibleInstances"
    ,"-XMultiParamTypeClasses"
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
        ,"soh-scheduler-api"
        ,"stack-prism"
        ,"stm"
        ,"text"
        ,"transformers"
        ,"uuid-types"
        ,"vector"
        ,"void"
        ])
    ["../soh-client"]
    "../soh-client/main.hs" )
