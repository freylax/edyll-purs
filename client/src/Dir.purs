module Dir where

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Array ( snoc, last, scanl)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Path (Path(..), DirListing(..), currentDir)
import Network.HTTP.Affjax (AJAX)
import Prelude (const, discard, show, ($), (<$>), (<<<), (<>))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (li, div, h1, button, ul)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)
import Type.Trout.Client (asClients)
import Edyll.Site ( site)

data Event = RequestDir (Array String)
           | ReceiveDir (Either Error DirListing)
           | RequestSubDir String
        
type State =
  { dir :: DirListing
  , status :: String }

init :: State
init = { dir: (DirListing [] []), status: "Nothing loaded yet"}

-- | Our update function requests data from the server, and handles data
-- | received from input.
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)

foldp (RequestDir p) state =
  { state: state { status = "Fetching dir..." }
  , effects: [ (Just <<< ReceiveDir) <$> attempt (dir p)."GET" ]
  }
  where {dir} = asClients site
foldp (RequestSubDir sub) state =
  { state: state { status = "Fetching dir..." }
  , effects: [ (Just <<< ReceiveDir) <$> attempt (dir $ cd <> [sub])."GET" ]
  }
  where
    {dir} = asClients site
    cd = currentDir state.dir

foldp (ReceiveDir d) state =
  noEffects $ case d of
    Left err -> state { dir = (DirListing [] []), status = "Error Fetching Dir:" <> show err }
    Right dir -> state { dir = dir, status = "Dir is here." }

view :: State -> HTML Event
view state =
  div do
    h1 $ text state.status
    div do
      button #! onClick (const $ RequestDir []) $ text "Dir"
      viewDir state.dir
      
viewDir :: DirListing -> HTML Event
viewDir (DirListing path l) =
  do
    div do
      text $ "Dir: "
      button ! className "undecorated" ! type' "button"
        #! onClick (const $ RequestDir []) $ text "."
      for_ (scanl snoc [] path) \p -> do
        text "/"
        button ! className "undecorated" ! type' "button"
          #! onClick (const $ RequestDir p)
          $ case last p of
              Just t -> text t
              Nothing -> text "?"
              
    ul $ for_ l viewPath

viewPath :: Path -> HTML Event
viewPath (Directory d) = li $ button ! className "undecorated" ! type' "button" #! onClick (const $ RequestSubDir d) $ text d
viewPath (File f) = li $ text f
  
