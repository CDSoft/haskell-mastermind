-- Minimalistic text user interface

module TUI where

import Control.Monad
import Data.Char
import Data.Maybe
import System.Console.Haskeline
import System.Exit
import System.IO

readLine :: String -> IO String
readLine prompt = do
    hFlush stdout
    maybeLine <-  runInputTBehavior defaultBehavior defaultSettings $ getInputLine prompt
    case maybeLine of
        Nothing -> exitSuccess
        Just line -> do
            let line' = (reverse . dropWhile isSpace . reverse . dropWhile isSpace) line
            return line'

menu :: [String] -> [(Char, String, IO a)] -> IO a
menu title items = do
    let snd (x,y,z) = y
    let len = maximum $ map length title ++ map ((+4).length.snd) items
    let bar = "/" ++ replicate (len+2) '=' ++ "\\"
    let bar' = "|" ++ replicate (len+2) '~' ++ "|"
    let items' = map (\(key,item,action) -> (toUpper key, take (len-4) (item ++ repeat ' '))) items
    let actions' = map (\(key,item,action) -> (toUpper key, action)) items
    let title' = map (\line -> take len (line ++ repeat ' ')) title
    putStrLn bar
    forM_ title' (\line -> putStrLn $ "| "++line++" |")
    putStrLn bar'
    forM_ items' (\(key, item) -> putStrLn $ "| ["++[key]++"] "++item++" |")
    putStrLn bar'
    key <- getKey $ map fst items'
    let item = fromMaybe undefined $ lookup key items'
    putStrLn ("| ==> "++item++" |")
    putStrLn $ reverse bar
    let action = fromMaybe undefined $ lookup key actions'
    action
    where
        getKey keys = do
            key <- readLine "|> "
            let key' = map toUpper key
            case key' of
                [k] | k `elem` keys -> return k
                _ -> getKey keys

demo :: IO String
demo = demo' ""
    where
        demo' name = menu ["Menu demo", "Hello "++name]
            [ ('n', "Name", do name' <- readLine "Your name: "
                               demo' name'
              ),
              ('q', "Quit", return name
              )
            ]
