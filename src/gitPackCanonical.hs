module Main where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import System.Process.ByteString (readProcessWithExitCode)

{-

- we can automatically include objects only directly reachable from a
  given commit. Not sure what to do with tags (both tag and oid should
  be arguments to pack builder).
- Use "git rev-list --objects" to avoid walking git graph directly - too complex
- git bindings are used for fetching objects, for performance reasons
- Sort the list
- Produce pack of objects ordered by oid
- Use idx v2 format, so we don't need to split our pack on 4GB boundary

-}


getObjects :: Config -> IO (Either String [ByteString])
getObjects


main :: IO ()
main = do
  pure ()
