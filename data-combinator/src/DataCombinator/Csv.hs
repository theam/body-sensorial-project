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

combine :: NonEmpty [RFrame Text Text] -> IO (RFrame Text Text)
combine frames
  = foldM extend (head frames) (tail frames)
  & fmap (f "elapsed (s)")
 where
  f = error "Sort not implemented"



-- TODO: MAKE A PR TO ANALYZE

-- | Checks that a key is contained in the 'RFrame'
isColumnOf :: (Analyze.Data k) => k -> RFrame k v -> Bool
isColumnOf key frame = Analyze.keepCols (== key) frame
                     & Analyze.numCols
                     & (/= 0)


-- | Extends an RFrame with another, adding empty values to missing columns
extend :: RFrame Text Text -> RFrame Text Text -> IO (RFrame Text Text)
extend a b = do
  let aKeys = Analyze.rframeKeys a
  let bKeys = Analyze.rframeKeys b
  let aNewKeys = Vector.filter (not . (`Vector.elem` aKeys)) (Analyze.rframeKeys a)
  let bNewKeys = Vector.filter (not . (`Vector.elem` bKeys)) (Analyze.rframeKeys b)
  extendedA <- foldM newEmptyColumn a bNewKeys
  extendedB <- foldM newEmptyColumn b aNewKeys
  Analyze.appendRows extendedA extendedB
 where
  newEmptyColumn frame newKey = Analyze.addColumn frame newKey (Vector.replicate (Analyze.numRows frame) "")
