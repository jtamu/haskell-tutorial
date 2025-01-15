{-# OPTIONS -Wall -Werror #-}

import Control.Exception
import Data.List
import System.Directory
import System.Environment
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExist command

main :: IO ()
main = do
  (command : argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "Usage: add filename 'todo item'"

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) ([0 ..] :: [Int]) todoTasks
  putStr $ unlines numberedTasks
view _ = putStrLn "Usage: view filename"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) ([0 ..] :: [Int]) todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError
    (openTempFile "." "temp")
    ( \(tmpName, tmpHandle) -> do
        hClose tmpHandle
        removeFile tmpName
    )
    ( \(tmpName, tmpHandle) -> do
        hPutStr tmpHandle newTodoItems
        hClose tmpHandle
        removeFile fileName
        renameFile tmpName fileName
    )
remove _ = putStrLn "Usage: remove filename number"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) ([0 ..] :: [Int]) todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      deletedTodoItems = delete (todoTasks !! number) todoTasks
      newTodoItems = unlines $ (todoTasks !! number) : deletedTodoItems
  bracketOnError
    (openTempFile "." "temp")
    ( \(tmpName, tmpHandle) -> do
        hClose tmpHandle
        removeFile tmpName
    )
    ( \(tmpName, tmpHandle) -> do
        hPutStr tmpHandle newTodoItems
        hClose tmpHandle
        removeFile fileName
        renameFile tmpName fileName
    )
bump _ = putStrLn "Usage: bump filename number"

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"
