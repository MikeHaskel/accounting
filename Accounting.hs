module Accounting where
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import Data.Tree
import Data.Char
import Data.Monoid
import Data.List
import Data.Ratio
import Data.Time
import System.IO
import System.IO.Error

type Path = (String,[String])
newtype PathSet = PathSet {getPathSet :: M.Map String PathSet}
                deriving Eq

checkPath :: MonadPlus m =>
             PathSet -> Path -> m ()
checkPath set (name,names) =
  do nextSet <- liftMaybe $ M.lookup name (getPathSet set)
     case names of
       [] -> return ()
       nextName:nextNames -> checkPath nextSet (nextName,nextNames)

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

isPrefixOfPath :: [String] -> Path -> Bool
isPrefixOfPath x (name,names) = isPrefixOf x (name:names)

type Amount = Rational

data Transaction =
  Transaction
  {
    transactionDescription :: String,
    transactionDate :: Day,
    transactionActivity :: Path,
    transactionSplits :: [Split]
  }
  
checkTransaction :: MonadPlus m =>
                    PathSet -> PathSet -> Transaction -> m ()
checkTransaction activities accounts t =
  do checkPath activities (transactionActivity t)
     mapM_ (checkSplit accounts) (transactionSplits t)
     assert $ sum (map splitDebitAmount $ transactionSplits t) == 0
     return ()
     
assert :: MonadPlus m => Bool -> m ()
assert True = return ()
assert False = mzero

data Split =
  Split
  {
    splitAccount :: Path,
    splitDebitAmount :: Amount
  }

checkSplit :: MonadPlus m =>
              PathSet -> Split -> m ()
checkSplit accounts s = checkPath accounts (splitAccount s)


-- This accumulates child amounts upstream
newtype PathAmounts =
  PathAmounts { getPathAmounts :: M.Map String (Amount, PathAmounts) }

emptyPathAmounts :: PathAmounts
emptyPathAmounts = PathAmounts M.empty

singletonPathAmounts :: Path -> Amount -> PathAmounts
singletonPathAmounts (name,names) amount =
  case names of
    [] -> PathAmounts $ M.singleton name (amount, emptyPathAmounts)
    nextName:nextNames ->
      PathAmounts $ M.singleton name (amount, singletonPathAmounts
                                              (nextName,nextNames)
                                              amount)

addPathAmounts :: PathAmounts -> PathAmounts -> PathAmounts
addPathAmounts (PathAmounts x) (PathAmounts y) =
  PathAmounts $
  M.unionWith
  (\(a1,p1) (a2,p2) -> (a1+a2,addPathAmounts p1 p2))
  x y

instance Monoid PathAmounts where
  mempty = emptyPathAmounts
  mappend = addPathAmounts


accountSheet :: (Day -> Bool) -> (Path -> Bool) ->
                [Transaction] -> PathAmounts
accountSheet dateFilter activityFilter transactions =
  mconcat [ singletonPathAmounts account amount |
            transaction <- transactions,
            dateFilter $ transactionDate transaction,
            activityFilter $ transactionActivity transaction,
            split <- transactionSplits transaction,
            let account = splitAccount split,
            let amount = splitDebitAmount split ]

activitySheet :: (Day -> Bool) -> (Path -> Bool) ->
                 [Transaction] -> PathAmounts
activitySheet dateFilter accountFilter transactions =
  mconcat [ singletonPathAmounts activity amount |
            transaction <- transactions,
            dateFilter $ transactionDate transaction,
            let activity = transactionActivity transaction,
            split <- transactionSplits transaction,
            accountFilter $ splitAccount split,
            let amount = splitDebitAmount split ]


readsPath :: ReadS Path
readsPath r = [ ((name,names),r) |
                ("[",r) <- lex r,
                (name:names,r) <- go ('.':r) ]
  where go r = [ (list,r) |
                 (x,r) <- lex r,
                 (list,r) <- case x of
                   "." -> [ (name:names,r) |
                            (name,r) <- readsIdent r,
                            (names,r) <- go r ]
                   "]" -> [ ([],r) ]
                   _ -> [] ]

showsPath :: Path -> ShowS
showsPath (name,names) = ("["++) .
                         (name++) .
                         foldr (\n c -> ("."++) . (n++) . c) ("]"++) names

readsIdent :: ReadS String
readsIdent r = [ (c:cs,r) |
                 (c:cs,r) <- lex r,
                 isAlpha c,
                 all isAlphaNum cs ]

readsAmount :: ReadS Amount
readsAmount = reads

showsAmount :: Amount -> ShowS
showsAmount amount = if amount >= 0
                     then go amount
                     else ("-"++) .
                          go (negate amount)
  where go amount =
          let thousandths = numerator amount * 1000 `div` denominator amount
          in shows (thousandths `div` 1000) .
             ("."++) .
             shows (thousandths `rem` 1000)

readsPathSet :: ReadS PathSet
readsPathSet r = [ (PathSet $ M.fromList list, r) |
                   ("{",r) <- lex r,
                   (list, r) <- go r ]
  where go r = [ ([],r) |
                 ("}",r) <- lex r ] ++
               [ ((name,children):rest,r) |
                 (name,r) <- readsIdent r,
                 (children,r) <- readsPathSet r,
                 (rest,r) <- go r ]

instance Read PathSet where
  readsPrec = const readsPathSet

showsPathSet :: PathSet -> ShowS
showsPathSet (PathSet set) =
  ("{ "++) .
  foldr
  (\(name,children) c ->
    (name++) . (" "++) . showsPathSet children . (" "++) . c) ("}"++)
  (M.toList set)

instance Show PathSet where
  showsPrec = const showsPathSet

pathSetToForest :: PathSet -> Forest String
pathSetToForest =
  map (\(name,children) -> Node name (pathSetToForest children)) .
  M.toList .
  getPathSet

prettyForest :: ShowS -> Forest ShowS -> ShowS
prettyForest linePrefix = (\forest -> case forest of
                              [] -> ("{}"++)
                              tree:trees -> ("{ "++) . go tree trees)
  where go tree [] = prettyTree (linePrefix . ("  "++)) tree . (" }"++)
        go tree (nextTree:nextTrees) =
          prettyTree (linePrefix . ("  "++)) tree .
          ("\n"++) . linePrefix . ("  "++) .
          go nextTree nextTrees

prettyTree :: ShowS -> Tree ShowS -> ShowS
prettyTree linePrefix (Node name []) = name .
                                       (" {}"++)
prettyTree linePrefix (Node name children) =
  name .
  ("\n"++) . linePrefix .
  prettyForest linePrefix children

prettyPathSet :: ShowS -> PathSet -> ShowS
prettyPathSet linePrefix = prettyForest linePrefix .
                           map (fmap (++)) .
                           pathSetToForest

showsPathAmounts :: PathAmounts -> ShowS
showsPathAmounts (PathAmounts set) =
  ("{ "++) .
  foldr
  (\(name,(amount,children)) c ->
    (name++) . (" "++) . showsAmount amount . (" "++) .
    showsPathAmounts children . (" "++) . c) ("}"++)
  (M.toList set)

instance Show PathAmounts where
  showsPrec = const showsPathAmounts

pathAmountsToForest :: PathAmounts -> Forest (String,Amount)
pathAmountsToForest =
  map (\(name,(amount,children)) ->
        Node (name,amount) (pathAmountsToForest children)).
  M.toList .
  getPathAmounts

prettyPathAmounts :: ShowS -> PathAmounts -> ShowS
prettyPathAmounts linePrefix = prettyForest linePrefix .
                               map (fmap (\(name,amount) ->
                                           (name++) .
                                           (" "++) .
                                           showsAmount amount)) .
                               pathAmountsToForest


readsSplit :: ReadS Split
readsSplit r = [ (Split account amount, r) |
                 (amount,r) <- readsAmount r,
                 (account,r) <- readsPath r ]

showsSplit :: Split -> ShowS
showsSplit (Split account amount) = showsAmount amount .
                                    (" "++) .
                                    showsPath account

instance Read Split where
  readsPrec = const readsSplit

instance Show Split where
  showsPrec = const showsSplit

readsTransaction :: ReadS Transaction
readsTransaction r = [ (Transaction descr date activity splits, r) |
                       (date, r) <- reads r,
                       (activity, r) <- readsPath r,
                       (descr, r) <- reads r,
                       (splits, r) <- reads r ]

showsTransaction :: Transaction -> ShowS
showsTransaction (Transaction descr date activity splits) =
  shows date . (" "++) .
  showsPath activity . (" "++) .
  shows descr . (" "++) .
  shows splits . (" "++)

instance Read Transaction where
  readsPrec = const readsTransaction

instance Show Transaction where
  showsPrec = const showsTransaction


loadTransactions :: (MonadIO m, MonadPlus m) =>
                    FilePath -> FilePath -> FilePath ->
                    m [Transaction]
loadTransactions activitiesFile accountsFile transactionsFile =
  do activities <- parseFile activitiesFile
     accounts <- parseFile accountsFile
     transactions <- parseFile transactionsFile
     mapM_ (checkTransaction activities accounts) transactions
     return transactions

parseFile :: (Read a, MonadIO m, MonadPlus m) =>
             FilePath -> m a
parseFile file =
  do r <- safeLiftIO $ readFile file
     foldr mplus mzero [ return x |
                         (x, r) <- reads r,
                         all isSpace r ]

liftEitherToPlus :: MonadPlus m =>
                    Either a b -> m b
liftEitherToPlus (Left _) = mzero
liftEitherToPlus (Right x) = return x

safeLiftIO :: (MonadIO m, MonadPlus m) =>
              IO a -> m a
safeLiftIO io = (liftIO $ tryIOError io) >>= liftEitherToPlus
