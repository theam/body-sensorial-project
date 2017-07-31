module DataCombinator.Wav where

import Foundation hiding ((<>))
import Foundation.Collection

import Data.Semigroup (Semigroup, (<>), sconcat)
import Control.Monad.Catch (MonadThrow)

import qualified Data.WAVE as Wave
import Data.WAVE (WAVE(..), WAVESample)

combine :: MonadThrow m => NonEmpty [WAVE] -> m WAVE
combine waves = do
    let baseWave = head waves
    return (foldr' (<>) baseWave $ getNonEmpty waves)

instance Semigroup WAVE where
    (WAVE header data1) <> (WAVE _ data2) = WAVE header (data1 <> data2)
