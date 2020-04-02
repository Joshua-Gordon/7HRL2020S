module Loader where

import qualified Data.Map as M
import Graphics.Gloss
import System.Directory
import Control.Monad

folderPath :: FilePath
folderPath = "./res"

loadAssets :: IO (M.Map String Picture)
loadAssets = do
  fileNames <- listDirectory folderPath
  foldM (\m filePath -> do
    asset <- loadBMP filePath
    return $ M.insert filePath asset m ) M.empty fileNames
