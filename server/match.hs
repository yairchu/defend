import System.Environment

main :: IO ()
main = do
  putStrLn . ("Content-type: text/plain\r\n\r\n" ++) =<< readFile hostFn
  writeFile hostFn =<< getEnv "QUERY_STRING"
  where
    hostFn = "/home/public/host.txt"

