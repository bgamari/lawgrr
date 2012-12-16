{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import Control.Error
import Data.Time.Clock
import Database.Persist
import Import
import Yesod.Form.Functions
import qualified Data.Text as T       

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

postFeedDeleteR :: Handler RepHtml
postFeedDeleteR = do
    defaultLayout [whamlet|Done|]

getFeedR :: FeedId -> Handler RepHtml
getFeedR feedId = do
    feed <- runDB $ get404 feedId
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle $ toHtml $ "Feed: "<>Import.feedName feed
        addScript $ StaticR js_d3_js
        addScript $ StaticR js_jquery_js
        $(widgetFile "feed")

getFeedEditR :: FeedId -> Handler RepHtml
getFeedEditR feedId = do
    feed <- runDB $ get404 feedId
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle $ toHtml $ "Edit Feed: "<>Import.feedName feed
        $(widgetFile "feed")

postFeedEditR :: FeedId -> Handler RepHtml
postFeedEditR feedId = do
    feed <- runDB $ get404 feedId
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle $ toHtml $ "Edit Feed: "<>Import.feedName feed
        $(widgetFile "feed")

postFeedR :: FeedId -> Handler RepHtml
postFeedR feedId = do
    feed <- runDB $ get404 feedId
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "feed")

postFeedPointsR :: FeedId -> Handler ()
postFeedPointsR feedId = do
    feed <- runDB $ get404 feedId
    key <- lookupPostParams "X-APIKey"
    feedKeys <- runDB $ selectList [ApiKeyFeedId ==. feedId] []
    case key of
        --[key] | key `elem` map (apiKeyKey . entityVal) feedKeys -> do
        otherwise -> do
            time <- liftIO $ getCurrentTime
            value <- lookupPostParams "value"
            case value of
                [] -> error "No value given"
                [value] | Just value' <- readMay (T.unpack value) -> do
                    pointId <- runDB $ insert
                               $ DataPoint { dataPointFeedId = feedId
                                           , dataPointTime = time
                                           , dataPointValue = value'
                                           }
                    return ()
        otherwise -> error "asdf"
    
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

