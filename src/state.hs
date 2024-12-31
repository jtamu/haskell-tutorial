import Control.Monad (forM_)
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.ST (runST)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put, state), MonadTrans (lift), State, StateT (StateT), evalState, execState, modify, runState, runStateT)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.STRef (modifySTRef, newSTRef, readSTRef)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
   in pop newStack2

popM :: State Stack Int
popM = state $ \(x : xs) -> (x, xs)

pushM :: Int -> State Stack ()
pushM x = state $ \xs -> ((), x : xs)

stackManipM :: State Stack Int
stackManipM = do
  pushM 3
  popM
  popM

stackStuff :: State Stack ()
stackStuff = do
  a <- popM
  if a == 5
    then pushM 5
    else do
      pushM 3
      pushM 8

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 3, 1]
    else put [9, 2, 1]

popM' :: State Stack Int
popM' = do
  xs <- get
  case xs of
    (x : ys) -> do
      put ys
      return x
    _ -> return 0

pushM' :: Int -> State Stack ()
pushM' x = do
  xs <- get
  put (x : xs)

main :: IO ()
-- main = ioRefFn
-- main = print =<< sum' [1 .. 5]
-- main = print $ sum'' [1 .. 5]
main = stateIO

ioRefFn :: IO ()
ioRefFn = do
  a <- newIORef 1
  writeIORef a 2
  r <- readIORef a
  print r

sum' :: [Int] -> IO Int
sum' xs = do
  s <- newIORef 0
  forM_ xs $ \x -> modifyIORef s (+ x)
  readIORef s

sum'' :: [Int] -> Int
sum'' xs = runST $ do
  s <- newSTRef 0
  forM_ xs $ \x -> modifySTRef s (+ x)
  readSTRef s

stateIO :: IO ()
stateIO = do
  let a = return 1 :: State s Int
  print $ runState a (1, 2)

sumState :: [Int] -> State Int Int
sumState [] = return 0
sumState (x : xs) = do
  modify (+ x)
  sumState xs

runSumState :: [Int] -> Int
runSumState xs = execState (sumState xs) 0

-- StateTモナド変換子
identityEx :: IO ()
identityEx = do
  let a = return 1 :: StateT s Identity Int
  print $ evalState a ()

a1 :: StateT s IO ()
a1 = StateT $ \s -> do
  a <- print "hello"
  return (a, s)

a2 :: StateT s IO ()
a2 = lift $ print "hello"

a :: StateT String IO ()
a = do
  v <- get
  lift $ print v

getch :: [a] -> (a, [a])
getch (x : xs) = (x, xs)

get3 :: [a] -> (a, a, a)
get3 xs0 =
  let (x1, xs1) = getch xs0
      (x2, xs2) = getch xs1
      (x3, xs3) = getch xs2
   in (x1, x2, x3)

getState :: State [a] a
getState = state getch

get3State :: State [a] [a]
get3State = do
  x1 <- getState
  x2 <- getState
  x3 <- getState
  return [x1, x2, x3]

maybeGetch :: [a] -> Maybe (a, [a])
maybeGetch (x : xs) = Just (x, xs)
maybeGetch _ = Nothing

maybeGetState :: StateT [a] Maybe a
maybeGetState = StateT maybeGetch

safeGet3State :: StateT [a] Maybe [a]
safeGet3State = do
  x1 <- maybeGetState
  x2 <- maybeGetState
  x3 <- maybeGetState
  return [x1, x2, x3]

-- 多重持ち上げ
test1 :: (Num a, Show a) => a -> IO ((), a)
test1 x = (`runStateT` x) $ do
  modify (+ 1)
  a <- get
  lift $ print a
  (`runReaderT` a) $ do
    b <- ask
    lift $ lift $ print $ b + 1

-- 多重持ち上げその２
test2 :: (Num a, Show a) => a -> IO ((), a)
test2 x = (`runStateT` x) $ do
  modify (+ 1)
  get >>= test3

test3 :: (Monad (t IO), MonadTrans t, Show a, Num a) => a -> t IO ()
test3 x = (`runReaderT` x) $ do
  b <- ask
  lift $ lift $ print $ b + 1

-- 多重持ち上げその３
test2' :: (Num a, MonadIO m, Show a) => a -> m ((), a)
test2' x = (`runStateT` x) $ do
  modify (+ 1)
  get >>= test3'

test3' :: (MonadIO m, Show a, Num a) => a -> m ()
test3' x = (`runReaderT` x) $ do
  b <- ask
  liftIO $ print $ b + 1

-- >>> stackManip [5,8,2,1]
-- (5,[8,2,1])
-- >>> runState stackManipM [5,8,2,1]
-- (5,[8,2,1])
-- >>> evalState stackManipM [5,8,2,1]
-- 5
-- >>> execState stackManipM [5,8,2,1]
-- [8,2,1]
-- >>> runState stackStuff [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])
-- >>> runSumState [1..5]
-- 15
-- >>> runStateT a "hello"
-- ((),"hello")
-- >>> runState get3State "abcdef"
-- ("abc","def")
-- >>> runState get3State "ab"
-- /opt/app/src/state.hs:122:1-24: Non-exhaustive patterns in function getch
-- >>> runStateT safeGet3State "abcdef"
-- Just ("abc","def")
-- >>> runStateT safeGet3State "ab"
-- Nothing
