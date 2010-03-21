import System.Environment

main :: IO ()
main = do
  query <- getEnv "QUERY_STRING"
  let
    hostDir = "/home/public/"
    bchar = '-'
    (gameVer, _:addrs) = break (== bchar) query
    (filename, addrsString)
      | bchar `elem` query = (hostDir ++ "host_" ++ gameVer ++ ".txt", addrs)
      | otherwise = (hostDir ++ "host.txt", query)
  putStr . ("Content-type: text/plain\r\n\r\n" ++) =<< readFile filename
  writeFile filename addrsString
