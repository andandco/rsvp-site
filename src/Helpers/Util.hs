module Helpers.Util where

import ClassyPrelude ((.), Text)
import System.IO.Unsafe (unsafePerformIO)
import S3 (presignAsset)

presign :: Text -> Text
presign = unsafePerformIO . presignAsset
