module RepCsv where

import Data.Csv
import qualified Data.Vector as V
import Prelude
import Yesod.Content
import Yesod.Handler
import Yesod.Json

typeCsv = "text/csv"

newtype RepCsv = RepCsv Content
instance HasReps RepCsv where
    chooseRep (RepCsv c) _ = return (typeCsv, c)

csvToRepCsv :: (ToNamedRecord a) => Header -> V.Vector a -> GHandler sub master RepCsv
csvToRepCsv header =
    return . RepCsv . toContent . encodeByName header

