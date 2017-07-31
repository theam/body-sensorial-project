module DataCombinator.Csv where

import Foundation
import Foundation.Collection
import Data.Function ((&))
import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow)

import qualified Analyze.Common as Analyze
import qualified Analyze.Csv as Analyze
import qualified Analyze.RFrame as Analyze
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Analyze.RFrame (RFrame)
import Data.Text (Text)
import Data.Vector (Vector)

combine :: (MonadThrow m) => NonEmpty [RFrame Text Text] -> m (RFrame Text Text)
combine frames = do
    let baseFrame = head frames
                  & Analyze.keepCols isTimeSeriesColumn
    let columns = getNonEmpty frames
                & fmap ( Analyze.dropCols (`isColumnOf` baseFrame) )
    foldM Analyze.extendCols baseFrame columns
  where
    isTimeSeriesColumn k = k `elem` [ "epoc (ms)", "timestamp (-05:00)", "elapsed (s)" ]

isColumnOf :: (Analyze.Data k) => k -> RFrame k v -> Bool
isColumnOf key frame = Analyze.keepCols (== key) frame
                     & Analyze.numCols
                     & (/= 0)