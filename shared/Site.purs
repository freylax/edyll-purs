module Edyll.Site where

import Data.Path (DirListing)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:=), type (:>), CaptureAll,
                   Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type DirResource = Resource (Get DirListing JSON)

type Site =
  "dir" := "dir" :/ CaptureAll "path" String :> DirResource

site :: Proxy Site
site = Proxy
