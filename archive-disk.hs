{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (when)
import Data.ByteString.Search.KnuthMorrisPratt (matchSS) -- from 'searchstring'
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)
import qualified Data.ByteString.Char8 as B -- (drop, empty, length, lines, pack, readFile, take, unpack, ByteString)
import System.Environment (getArgs)
import System.IO.Unsafe -- (unsafeInterleaveIO)

import Codec.Binary.UTF8.String (encodeString)
import System.Directory -- (executable, getTemporaryDirectory, removeFile,)
import System.Exit (ExitCode(..))

import System.IO (openTempFile)
import System.Process (runProcess, waitForProcess)
import Foreign.Ptr
import qualified Data.ByteString.Internal   as BI                                                                
import Foreign.C.Types
import Data.Word
import Foreign.ForeignPtr




main :: IO ()
main = do args <- getArgs
          filepaths <- case args of
                       (a:_) ->getFiles a
                       [] -> getFiles "./"
          files <- mapM (unsafeInterleaveIO . maybeRead) filepaths
          let urls = map B.unpack $ concatMap checkurls files
          mapM_ (archive "gwern@mailinator.com") urls

-- SPIDERING
------------------
-- I'm lazy - shell out
-- | Run shell command and return error status, standard output, and error output.  Assumes
-- UTF-8 locale. Note that this does not actually go through \/bin\/sh!
runShellCommand :: FilePath                     -- ^ Working directory
                -> Maybe [(String, String)]     -- ^ Environment
                -> String                       -- ^ Command
                -> [String]                     -- ^ Arguments
                -> IO (ExitCode, B.ByteString, B.ByteString)
runShellCommand workingDir environment command optionList = do
  tempPath <- catch getTemporaryDirectory (\_ -> return ".")
  (outputPath, hOut) <- openTempFile tempPath "out"
  (errorPath, hErr) <- openTempFile tempPath "err"
  hProcess <- runProcess (encodeString command) (map encodeString optionList) (Just workingDir) environment Nothing (Just hOut) (Just hErr)
  status <- waitForProcess hProcess
  errorOutput <- B.readFile errorPath
  output <- B.readFile outputPath
  removeFile errorPath
  removeFile outputPath
  return (status, errorOutput, output)

getFiles :: FilePath -> IO [FilePath]
getFiles x = do (_,_,c) <- runShellCommand "./" Nothing "find" [x, "-type", "f"]
                return $ map B.unpack $ B.lines c
          --       where readp a = when (isText a) $ a


maybeRead :: FilePath -> IO B.ByteString
maybeRead x = do perms <- getPermissions x
                 if executable perms || not (readable perms) then return B.empty
                  else do
                     text <- fmap not $ isText x 
                     putStrLn $ x ++ show text
                     if text then B.readFile x else return B.empty 

-- checks whether a file is made of binary or textual data
isText :: FilePath -> IO Bool
isText f = fmap is_funky $ B.readFile f

{-# INLINE is_funky #-}
is_funky :: B.ByteString -> Bool
is_funky ps = case BI.toForeignPtr ps of
   (x,s,l) ->
    unsafePerformIO $ withForeignPtr x $ \p->
    (/=0) `fmap` has_funky_char (p `plusPtr` s) (fromIntegral l)

foreign import ccall unsafe "fpstring.h has_funky_char" has_funky_char
    :: Ptr Word8 -> CInt -> IO CInt
------------------

-- PARSING
------------------

-- | Parse a bytestring for all plausible HTTP URLs
hits :: B.ByteString -> [B.ByteString]
hits file = let indices = starts file in
            let prefixes = map (head . B.lines . flip B.drop file . fromIntegral) indices in
            concatMap checkurls prefixes

starts :: B.ByteString -> [Int64]
starts = matchSS (B.pack "http://")

-- Brute force. Take everything that could possibly be a URI.
checkurls :: B.ByteString -> [B.ByteString]
checkurls b = check 1
             where check n = let try = B.take n b in
                                 if B.length b < n then [] else
                                   if isURI (B.unpack try) && B.length try > 7 -- "http://"
                                   then try : check (n+1)
                                   else check (n+1)
------------------

-- ARCHIVING
------------------ stolen from my gitit plugin WebArchiver.hs
-- | Error check the URL and then archive it both ways
archive :: String -> String -> IO () -- removed check because we did that already
archive email url = print url >> webciteArchive email url >> alexaArchive url

webciteArchive :: String -> String -> IO ThreadId
webciteArchive email url = forkIO (ignore $ openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email))
   where openURL = simpleHTTP . getRequest
         ignore = fmap $ const ()

alexaArchive :: String -> IO ()
alexaArchive url = do let archiveform = Form POST (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                                                             [("url", url), ("submit", "")]
                      (uri, resp) <- browse $ request $ formToRequest archiveform
                      when (uriPath uri /= "/help/crawlthanks") $
                           error $ "Request failed! Did Alexa change webpages? Response:" ++ rspBody resp
------------------