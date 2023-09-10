module Utils
  ( basePath,
  )
where

import Data.Functor
import Language.Haskell.TH
import System.Directory
import System.FilePath

-- Path of the project base
basePath :: FilePath
basePath =
  $( location
       >>= runIO . makeAbsolute . loc_filename
       <&> LitE . StringL . takeDirectory . takeDirectory
   )
