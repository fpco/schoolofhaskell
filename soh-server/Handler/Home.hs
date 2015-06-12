module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "SoH"
    $(widgetFile "homepage")
    $(widgetFile "editor")
    toWidget [hamlet|<script src=@{EditorScriptR}>|]
