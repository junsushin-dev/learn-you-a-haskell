import System.Environment
import System.IO
import System.Directory
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError, isFullError, isIllegalOperation, ioeGetFileName)

-- main = do
--   (fileName:_) <- getArgs
--   fileExists <- doesFileExist fileName
--   if fileExists
--     then do contents <- readFile fileName
--             putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--     else do putStrLn "The file doesn't exist!"

main = toTry `catch` handler

toTry :: IO ()
toTry = do 
  (fileName:_) <- getArgs  
  contents <- readFile fileName  
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e 
  | isDoesNotExistError e = 
      case ioeGetFileName e of
        Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
        Nothing -> putStrLn $ "Whoops! File does not exist at unknown location!"
  | otherwise = ioError e