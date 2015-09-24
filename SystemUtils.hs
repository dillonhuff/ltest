module SystemUtils(runCommandStrict,
                   readFileShowingContents) where

import System.IO.Strict as S
import System.Process

runCommandStrict :: String -> IO ()
runCommandStrict str = do
  putStrLn $ "\n" ++ str
  cmdHandle <- runCommand str
  waitForProcess cmdHandle
  return ()

readFileShowingContents :: String -> IO String
readFileShowingContents fileName = do
  putStrLn $ "Reading result file " ++ fileName ++ "\n"
  res <- S.readFile fileName
  putStrLn $ show res
  return res

