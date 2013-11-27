module FileUtils where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.ByteString.Base64 as B64

getFileLines :: FilePath -> IO [String]
getFileLines path = do
                        contents <- readFile path
                        return (lines contents)

saveBase64File :: T.Text -> IO()
saveBase64File b64 = do
                        putStrLn $ B.unpack <$> B64.decode $ B.fromString $ T.unpack $ b64
