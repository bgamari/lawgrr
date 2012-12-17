{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Keys where

import           Data.Random.Source (getRandomNByteInteger)
import           Data.Random.Source.IO ()
import qualified Data.Text as T
import           Database.Persist
import           Import
import           Numeric (showHex)

apiKeysWidget :: FeedId -> [ApiKey] -> Widget
apiKeysWidget feedId apiKeys = $(widgetFile "api-keys")
    
postFeedKeysR :: FeedId -> Handler RepHtmlJson
postFeedKeysR feedId = do
    feed <- runDB $ get404 feedId
    key <- liftIO $ (\x->T.pack $ showHex x "")
                 <$> getRandomNByteInteger 16
    runDB $ insert $ ApiKey feedId key
    let widget = do setTitle "New key"
                    [whamlet|<p>Your new key is #{key}|]
        json = object [ "key" .= key ]
    defaultLayoutJson widget json

deleteFeedKeyR :: FeedId -> Text -> Handler RepHtml
deleteFeedKeyR feedId apiKey = do
    let key = UniqueApiKey feedId apiKey
    runDB $ getBy404 key >> deleteBy key
    defaultLayout $ return ()

