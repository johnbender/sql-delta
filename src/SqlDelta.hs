import System.Environment

main :: IO ()
main = getArgs >>= print . go . head

go s = "sql-parser " ++ s