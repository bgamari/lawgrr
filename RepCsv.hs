{-# LANGUAGE StandaloneDeriving #-}
module RepCsv where

import Prelude
import Data.Monoid (Endo)
import Control.Monad.Trans.Writer (Writer)
import Data.Csv
import qualified Data.Vector as V
import Yesod hiding (Header)
import Yesod.Core.Content

newtype RepCsv = RepCsv Content

typeCsv = "text/csv"
instance HasContentType RepCsv where
    getContentType _ = typeCsv
deriving instance ToContent RepCsv

instance ToTypedContent RepCsv where
    toTypedContent (RepCsv c) = TypedContent typeCsv c

returnCsv :: (Monad m, ToNamedRecord a)
          => Header -> V.Vector a -> m RepCsv
returnCsv header =
    return . RepCsv . toContent . encodeByName header

provideCsv :: (Monad m, ToNamedRecord a)
           => Header -> V.Vector a -> Writer (Endo [ProvidedRep m]) ()
provideCsv header =
    provideRep . returnCsv header           
          
