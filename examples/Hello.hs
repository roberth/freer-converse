-- | A simple console program with getLine and putLine calls.
module Hello where
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal (send)
import           Data.Monoid
import           Data.Text                    (Text)
import           Prelude                      hiding (getLine)

data Console a where
  GetLine :: Console Text
  PutLine :: Text -> Console ()

putLine :: Member Console r => Text -> Eff r ()
putLine = send . PutLine

getLine :: Member Console r => Eff r Text
getLine = send GetLine

hello :: Member Console r => Eff r ()
hello = do
  putLine "Who are you?"
  name <- getLine
  putLine $ "Hello, " <> name
