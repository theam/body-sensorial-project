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
combine frames = do
  x <- foldM extend (head frames) (tail frames)
  return x



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
  putStrLn "Getting A new keys"
  let aNewKeys = Vector.filter (not . (`Vector.elem` bKeys)) (Analyze.rframeKeys a)
  putStrLn $ show $ aNewKeys
  putStrLn "Getting B new keys"
  let bNewKeys = Vector.filter (not . (`Vector.elem` aKeys)) (Analyze.rframeKeys b)
  putStrLn $ show $ bNewKeys
  putStrLn "Extending A"
  extendedA <- foldM newEmptyColumn a bNewKeys
  putStrLn "Extending B"
  extendedB <- foldM newEmptyColumn b aNewKeys
  putStrLn $ show $ Analyze.rframeKeys extendedA
  putStrLn $ show $ Analyze.rframeKeys extendedB
  xtended <- Analyze.appendRows extendedA extendedB
  putStrLn "-----------------------------------------"
  putStrLn $ "[ LENGTH: " <> (show $ Analyze.numRows xtended) <> " ]"
  putStrLn $ show $ Analyze.rframeKeys xtended
  -- Vector.mapM_ (putStrLn . show) $ Analyze.rframeData xtended
  putStrLn "-----------------------------------------"
  return xtended
 where
  newEmptyColumn frame newKey = Analyze.addColumn frame newKey (Vector.replicate (Analyze.numRows frame) "")
