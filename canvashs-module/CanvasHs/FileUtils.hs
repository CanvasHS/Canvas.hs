module FileUtils where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Base64 as B64
import Control.Monad.Trans (liftIO, lift)

getFileLines :: FilePath -> IO [String]
getFileLines path = do
                        contents <- readFile path
                        return (lines contents)

saveBase64File :: T.Text -> IO()
saveBase64File b64 = do
                        case (B64.decode $ B.fromString $ T.unpack $ b64) of
                            (Left a) -> putStrLn a
                            (Right b) -> putStrLn $ B.toString b
