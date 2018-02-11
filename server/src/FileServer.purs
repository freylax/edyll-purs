module Edyll.FileServer (rwFileServer) where

import Prelude

import Control.IxMonad (ibind, (:*>))
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, head, tail)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.String (splitAt, length)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Request ( class Request, class ReadableBody
                     , getRequestData, readBody)
import Hyper.Response ( class Response, class ResponseWritable
                      , ResponseEnded, StatusLineOpen, closeHeaders
                      , end, headers, send, toResponse, writeStatus)
import Hyper.Status (statusOK)
import Node.Buffer as Buffer
import Node.Buffer (BUFFER, Buffer)
import Node.FS (FS)
import Node.FS.Aff (exists, readFile, stat, unlink, writeFile)
import Node.FS.Stats ( isFile)
import Node.Path (FilePath)
import Node.Path as Path

serveFile
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => ResponseWritable b m Buffer
  => Response res m b
  => FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
serveFile path = do
  buf <- lift' (liftAff (readFile path))
  contentLength <- liftEff (Buffer.size buf)
  _ <- writeStatus statusOK
  _ <- headers [ Tuple "Content-Type" contentType
          , Tuple "Content-Length" (show contentLength)
          ]
  response <- toResponse buf
  _ <- send response
  end
  where
    bind = ibind
    contentType =
      case takeLast 4 path of
        ".css" -> "text/css; charset=utf-8"
        _ -> "*/*; charset=utf-8"
    takeLast :: Int -> String -> String
    takeLast n s = case splitAt ((length s) - n) s of
      Just { after:tail } -> tail
      Nothing -> ""

deleteFile
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS | e) m
  => Response res m b
  => FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
deleteFile path = 
  lift' (liftAff (unlink path))
  :*> writeStatus statusOK 
  :*> closeHeaders
  :*> end
  
saveFile
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Request req m
  => ReadableBody req m Buffer
  => Response res m b
  => FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
saveFile path = do 
  conn <- getConn
  {headers} <- getRequestData
  body <- readBody
  _ <- lift' (liftAff (writeFile path body))
  _ <- writeStatus statusOK
  _ <- closeHeaders
  end -- respond "OK"
  where
    bind=ibind
    
-- | Extremly basic implementation of static file serving. Needs more love.
rwFileServer
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Request req m
  => ReadableBody req m Buffer
  => ResponseWritable b m Buffer
  => Response res m b
  => String
  -> FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
rwFileServer routePrefix dir on404 = do
  conn ← getConn
  ctx <- context <$> getRequestData
  case head ctx.path, tail ctx.path of
    (Just h), (Just p) | h == routePrefix 
                           -> serve ctx.method (Path.concat (cons dir p))
                       | otherwise -> on404
    _ , _ -> on404
  where
    context { parsedUrl, method } =
      let parsedUrl' = force parsedUrl in
      { path: parsedUrl'.path
      , method: method
      }
  
    serve method absolutePath =
      case method of
        Left GET -> do
          fExists <- lift' (liftAff (exists absolutePath))
          if fExists 
            then do
              stats <- lift' (liftAff (stat absolutePath))
              if isFile stats 
                then serveFile absolutePath
                else on404
            else on404
        Left DELETE -> deleteFile absolutePath
        Left PUT -> saveFile absolutePath
        _  -> on404

    bind = ibind
