test -d bin || mkdir bin

ghc --make Check.hs -o bin/check
ghc --make BalanceSheet.hs -o bin/balance_sheet
ghc --make CashFlow.hs -o bin/cash_flow
ghc --make GainLoss.hs -o bin/gain_loss
