module Loader where

import qualified Data.Map as M
import Graphics.Gloss
import System.Directory
import Control.Monad
import Data.Function
import Data.List

folderPath :: FilePath
folderPath = "./res"

isSuffix :: Eq a => [a] -> [a] -> Bool
isSuffix = on isPrefixOf reverse

loadAssets :: IO (M.Map String Picture)
loadAssets = do
  fileNames <- listDirectory folderPath
  foldM (\m filePath -> do
    asset <- loadBMP (folderPath ++ "/" ++ filePath)
    return $ M.insert filePath asset m ) M.empty (filter (isSuffix ".bmp") fileNames)
