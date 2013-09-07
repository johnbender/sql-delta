import System.Environment
import Database.SqlDelta.Core (reconcile)

main :: IO ()
main = do
  [old, new] <- getArgs
  result <- reconcile old new
  print result
