module Main where
import Accounting
import Control.Monad.Trans.List
import System.IO
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     let dateFilter = case args of
           [beginStr,endStr] ->
             let begin = read beginStr
                 end = read endStr
             in (\date -> date >= begin && date < end)
     [tx] <- runListT $
             loadTransactions "activities" "accounts" "transactions"
     let sheet = activitySheet
                 dateFilter
                 (isPrefixOfPath ["Assets","Cash"])
                 tx
     putStrLn $ prettyPathAmounts id sheet ""
