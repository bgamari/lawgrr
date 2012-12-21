{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import           Control.Error
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import           Database.Persist
import           Import hiding (parseTime)
import           System.Locale
import           Yesod.Form.Functions
import           Control.Monad ((>=>))
import           Handler.Keys
import           Data.Time.Format.Human (humanReadableTime)

feedLastUpdate :: FeedId -> Handler (Maybe UTCTime)
feedLastUpdate feedId =
    (fmap (dataPointTime . entityVal) . listToMaybe)
    <$> runDB (selectList [ DataPointFeedId ==. feedId ] [ Desc DataPointTime, LimitTo 1 ])

humanFeedLastUpdate :: FeedId -> Handler (Maybe String)
humanFeedLastUpdate =
    feedLastUpdate >=> maybe (return Nothing) (\t->Just <$> liftIO (humanReadableTime t))

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
    lastUpdate <- humanFeedLastUpdate feedId
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
    
parseParam :: Read a => Text -> Text -> (String -> Maybe a) -> Handler a
parseParam err p read = do
    p' <- lookupGetParams p
    case p' of
        []  -> invalidArgs ["Missing "<>p]
        a:_ -> maybe (invalidArgs [err]) return $ read $ T.unpack a

parseDateParam :: Text -> Text -> Handler UTCTime
parseDateParam err p = do               
    parseParam err p (parseTime defaultTimeLocale fmt)
    where fmt = iso8601DateFormat (Just "%H:%M:%S%Q%Z")

getFeedPointsR :: FeedId -> Handler RepJson
getFeedPointsR feedId = do
    feed <- runDB $ get404 feedId
    period <- parseParam "Couldn't parse period" "period" readMay :: Handler Double
    start <- parseDateParam "Couldn't parse start" "start"
    end <- parseDateParam "Couldn't parse end" "end"
    let q = [ DataPointFeedId ==. feedId
            , DataPointTime >=. start
            , DataPointTime <=. end
            ]
    let decimate [] = []
        decimate (p:rest) =
            let diff = realToFrac $ secondsToDiffTime $ round $ period/1000
            in p:decimate (dropWhile (\p'->dataPointTime p' < (diff `addUTCTime` dataPointTime p)) rest)
    points <- (decimate . map entityVal) <$> runDB (selectList q [])
    let pointToObj p = object [ "time" .= dataPointTime p
                              , "value" .= dataPointValue p
                              ]
        json = object [ "name" .= feedName feed
                      , "units" .= feedUnits feed
                      , "points" .= map pointToObj points
                      ]
    jsonToRepJson json 
 
