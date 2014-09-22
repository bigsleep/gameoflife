module Root
( rootApp
) where

import qualified Network.Wai as Wai (Application, responseFile)
import qualified Network.HTTP.Types as HTTP (status200)


rootApp :: Wai.Application
rootApp _ respond =
    respond $ Wai.responseFile HTTP.status200 [] "index.html" Nothing

