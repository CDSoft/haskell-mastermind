# License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see http://www.gnu.org/licenses/.

# Introduction

A long long time ago I wrote a Mastermind game in Pascal. And I recently
watched a video of Peter Marks about [lazy evaluation in
Haskell](https://www.youtube.com/watch?v=eSrVC4-p7aI). Mastermind is
used as an example of lazy evaluation.

So here is my own version…

The user interface is a small and ugly text interface. It’s source code
is here: [TUI.hs](TUI.hs).

``` haskell
module Main where

import Data.List
import Data.Char
import Control.Monad
import System.Random
import System.IO
import TUI
```

# Pegs

The original game has pegs of six different colors. Pegs are here
represented by letters from `a` to `f`. The secret code to guess is made
of 4 pegs.

``` haskell
pegs :: [Char]
pegs = ['a'..'f']

nbPegs :: Int
nbPegs = 4
```

# Game intelligence

First we need a function to generate all the possible combinations. The
first combination is, arbitrarily, `"abcd"`.

``` haskell
type Guess = [Char]

allGuesses :: Int -> [Guess]
allGuesses n = init : filter (/=init) (all n)
    where
        init = take n $ cycle pegs
        all 0 = [[]]
        all n = [p:ps | p <- pegs, ps <- all (n-1)]
```

Then a function to evaluate a guess. A score is a tuple of integers
indication the number of pegs in the right position and the number of
pegs in wrong positions:

- `right` is computed by *zipping* the secret code and the guess and
  counting pegs that are equal.
- `wrong` is then the number of pegs in `secret` but not in the right
  places. The trick is that `secret \\ guess` removes one and only one
  item in `secret` for each item in `guess`.

``` haskell
type Score = (Int, Int)

calcScore :: Guess -> Guess -> Score
calcScore secret guess = (right, wrong)
    where
        right = sum [1 | (x,y) <- zip secret guess, x == y]
        notFound = secret \\ guess
        wrong = length secret - length notFound - right
```

To let the computer play, it must be able to filter the current list of
guesses to keep only the guesses that give the same score than the ones
previously played. `guess` is the last guess made by the computer and
`(right, wrong)` its score given by the opponent.

``` haskell
makeGuess :: Guess -> Score -> [Guess] -> [Guess]
makeGuess guess (right, wrong) =
    filter (\g -> calcScore g guess == (right, wrong))
```

The human player has to give scores to the computer. Here is a function
to parse a score. A score is a string of two digits. Both digits and
their sum must be in `[0, nbPegs]`. If the input string is not valid,
the function returns `Nothing` so that the user can retry entering a
valid score.

``` haskell
parseScore :: String -> Maybe Score
parseScore [r, w]
    | valid right && valid wrong && valid (right+wrong)
        = Just (right, wrong)
    where valid n = 0 <= n && n <= nbPegs
          right = ord r - ord '0'
          wrong = ord w - ord '0'
parseScore _ = Nothing
```

`randomPegs` is an infinite list of randomly choosen pegs. It’s used by
`randomSecret` to make a list of `nbPegs` rangom pegs.

``` haskell
randomPegs :: [IO Char]
randomPegs = fmap ( (pegs!!).(`mod`length pegs) ) randomIO : randomPegs

randomSecret :: IO Guess
randomSecret = sequence $ take nbPegs randomPegs
```

# User Interface

## Human player

The human player has to guess the secret code choosen by the computer.

He enters its guesses as strings of `nbPegs`. The computer computes and
show the score of the human guess until he finds the secret code.

``` haskell
human :: IO ()
human = do
    putStrLn ""
    secret <- randomSecret
    humanTurn 1 secret

humanTurn :: Int -> Guess -> IO ()
humanTurn n secret = do
    guess <- readLine $ "Human turn " ++ show n ++ ": "
    if length guess /= nbPegs
        then humanTurn n secret
        else do
            let (right, wrong) = calcScore secret guess
            putStrLn $ "score: " ++ show right ++ "-" ++ show wrong
            if right == nbPegs
                then putStrLn "Congratulation!"
                else humanTurn (n+1) secret
```

## Computer player

The computer player tries to guess the secret code choosen by the human
player.

For each guess made by the computer, the human player has to enter a
score. The computer filters its guesses until it finds the secret code.

``` haskell
computer :: IO ()
computer = do
    putStrLn ""
    computerTurn 1 $ allGuesses nbPegs

computerTurn :: Int -> [Guess] -> IO ()
computerTurn n (guess:guesses) = do
    --putStrLn $ "Possible guesses: " ++ show (1 + length guesses)
    s <- readLine $ "Computer turn " ++ show n ++ ": " ++ show guess ++ " => "
    let score = parseScore s
    case score of
        Nothing -> computerTurn n (guess:guesses)
        Just (right, wrong) ->
            if right < nbPegs
                then computerTurn (n+1) $ makeGuess guess (right, wrong) guesses
                else putStrLn "I'm the best!"

computerTurn n [] =
    putStrLn "You, cheater! You've made a mistake, haven't you?"
```

## Both players managed by the computer

The computer plays against itself. This mode is for lazy humans :-)

``` haskell
both :: IO ()
both = do
    putStrLn ""
    secret <- randomSecret
    bothTurn secret 1 $ allGuesses nbPegs

bothTurn :: Guess -> Int -> [Guess] -> IO ()
bothTurn secret n (guess:guesses) = do
    --putStrLn $ "Possible guesses: " ++ show (1 + length guesses)
    putStr $ "Computer turn " ++ show n ++ ": " ++ show guess ++ " => "
    hFlush stdout
    let (right, wrong) = calcScore secret guess
    putStrLn $ show right ++ show wrong
    if right < nbPegs
        then bothTurn secret (n+1) $ makeGuess guess (right, wrong) guesses
        else putStrLn "I'm the best!"
```

# Main menu

``` haskell
main :: IO ()
main = do
    putStrLn ""
    menu ["Mastermind in Haskell"]
        [   ('H', "Human player", human >> main),
            ('C', "Computer player", computer >> main),
            ('B', "Both players are computers", both >> main),
            ('Q', "Quit", return ())
        ]
    putStrLn ""
```

# Example

Let’s see how the computer plays…

    $ runhashell mastermind.lhs


    /================================\
    | Mastermind in Haskell          |
    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    | [H] Human player               |
    | [C] Computer player            |
    | [B] Both players are computers |
    | [Q] Quit                       |
    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    |> | ==> Both players are computers |
    \================================/

    Computer turn 1: "abcd" => 11
    Computer turn 2: "aaab" => 10
    Computer turn 3: "acec" => 11
    Computer turn 4: "adde" => 21
    Computer turn 5: "aedf" => 40
    I'm the best!

    /================================\
    | Mastermind in Haskell          |
    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    | [H] Human player               |
    | [C] Computer player            |
    | [B] Both players are computers |
    | [Q] Quit                       |
    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    |> | ==> Quit                       |
    \================================/
