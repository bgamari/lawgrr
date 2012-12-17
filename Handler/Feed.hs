{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import           Control.Error
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.Persist
import           Import
import           Yesod.Form.Functions

import           Handler.Keys

getFeedsR :: Handler RepHtml
getFeedsR = do
    feeds <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Feeds"
        $(widgetFile "feeds")

feedForm :: Form Feed
feedForm = renderDivs $ Feed
    <$> areq textField "Name" Nothing
    <*> areq htmlField "Description" Nothing
    <*> areq textField "Units" Nothing

getFeedAddR :: Handler RepHtml
getFeedAddR = do
    (widget, enctype) <- generateFormPost feedForm
    defaultLayout [whamlet|
<form method=post action=@{FeedAddR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

postFeedAddR :: Handler RepHtml
postFeedAddR = do
    ((result,widget), enctype) <- runFormPost feedForm
    case result of
        FormSuccess feed -> do
            feedId <- runDB $ insert feed
            getFeedR feedId
        otherwise -> defaultLayout [whamlet|Done|]

getFeedR :: FeedId -> Handler RepHtml
getFeedR feedId = do
    feed <- runDB $ get404 feedId
    apiKeys <- runDB $ selectList [ ApiKeyFeedId ==. feedId ] []
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle $ toHtml $ "Feed: "<>Import.feedName feed
        addScript $ StaticR js_d3_js
        addScript $ StaticR js_jquery_js
        $(widgetFile "feed")
        [whamlet|
<div>
    <h2>
        Keys
        ^{apiKeysWidget feedId (map entityVal apiKeys)}
|]

deleteFeedR :: FeedId -> Handler RepHtml
deleteFeedR feedId = do
    runDB $ get404 feedId >> delete feedId
    defaultLayout [whamlet|done|]

postFeedPointsR :: FeedId -> Handler ()
postFeedPointsR feedId = do
    feed <- runDB $ get404 feedId
    key <- lookupPostParams "key"
    feedKeys <- runDB $ selectList [ApiKeyFeedId ==. feedId] []
    case key of
        [key] | key `elem` map (apiKeyKey . entityVal) feedKeys -> do
            time <- liftIO $ getCurrentTime
            value <- lookupPostParams "value"
            case value of
                [] -> invalidArgs ["Value not given"]
                [value] | Just value' <- readMay (T.unpack value) -> do
                    pointId <- runDB $ insert
                               $ DataPoint { dataPointFeedId = feedId
                                           , dataPointTime = time
                                           , dataPointValue = value'
                                           }
                    return ()
        otherwise -> permissionDenied "Need API key to submit data points"
    
getFeedPointsR :: FeedId -> Handler RepJson
getFeedPointsR feedId = do
    feed <- runDB $ get404 feedId
    points <- runDB $ selectList [DataPointFeedId ==. feedId] []
    let pointToObj p = object [ "time" .= dataPointTime p
                              , "value" .= dataPointValue p
                              ]
        json = object [ "name" .= feedName feed
                      , "units" .= feedUnits feed
                      , "points" .= map (pointToObj . entityVal) points
                      ]
    jsonToRepJson json 
 
