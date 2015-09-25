import Text.Lucius
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main = do
  input <- LT.readFile "soh-client/soh.lucius"
  case luciusRT input [] of
    Left err -> fail err
    Right res -> LT.writeFile "demo/soh.css" res
