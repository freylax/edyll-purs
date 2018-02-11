module Server where

import Prelude

import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (intercalate)
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Data.Path (DirListing(..), Path(..))
import Data.Traversable (for)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Hyper.Trout.Router (RoutingError, router)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.HTTP (HTTP)
import Node.Path (FilePath)
import Edyll.FileServer (rwFileServer)
import Edyll.Site ( site)

type AppM e a = ExceptT RoutingError (Aff e) a --- (ReaderT (Array Task) (Aff e)) a


myReadDir :: forall e. (Aff (fs :: FS | e) ) (Array FilePath)
myReadDir = pure []


dirResource :: forall e. (Array String)
               -> {"GET" ::AppM (console :: CONSOLE, fs :: FS | e) DirListing}
dirResource dir = {"GET": do
                      let aDir = "../file" <> intercalate "/" dir
                      liftEff $ ( log "dirResource" )
                      d <- lift ( readdir aDir )
                      liftEff $ ( log $ "dir is " <> ( show  d ) )
                      l <- for d \f -> do
                          s <- lift $ stat $ aDir <> "/" <> f
                          pure if isDirectory s then Directory f
                               else File f
                      pure (DirListing dir l)
                      
                  }                 


    
main :: forall e. Eff (http :: HTTP, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER, fs :: FS | e) Unit
main =
  runServer' defaultOptionsWithLogging {} id siteRouter --- flip runReaderT tasks) siteRouter
  where
    resources = { dir: dirResource
                }
    siteRouter = router site resources onRoutingError
      
    notFound =
      writeStatus statusNotFound
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond "<h1>I have Not Found</h1>"

    onRoutingError status ms
      | status == statusNotFound = rwFileServer "file" "../file" notFound'
      | otherwise =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id ms)

    notFound' =
      fileServer "../client/public" notFound
     
