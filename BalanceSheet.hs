module Main where
import Accounting
import Control.Monad.Trans.List
import System.IO
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     let dateFilter = case args of
           [] -> const True
           [date] -> (< read date)
     [tx] <- runListT $
             loadTransactions "activities" "accounts" "transactions"
     let sheet = accountSheet dateFilter (const True) tx
     putStrLn $ prettyPathAmounts id sheet ""
