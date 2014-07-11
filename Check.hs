module Main where
import Accounting
import Control.Monad.Trans.List
import System.IO
import System.Exit

main :: IO ()
main =
  do list_txs <- runListT $
                 loadTransactions "activities" "accounts" "transactions"
     case list_txs of
       [tx] -> exitSuccess
       _ -> exitFailure
