module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    let devMode =
#if DEVELOPMENT
          True
#else
          False
#endif
    setTitle "SoH"
    $(widgetFile "homepage")
    $(widgetFile "editor")
    toWidget [hamlet|<script src=@{EditorScriptR}>|]
