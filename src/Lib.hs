{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where


import Prelude as P
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Data.Set as S



data RegEx = Literal Char
           | Concatenation [RegEx]
           | Union [RegEx]
           | Repetition RegEx
          deriving (Eq, Show)

literalParser :: GenParser Char st RegEx
literalParser = do ch <- alphaNum
                   m <- optionMaybe $ char '*'
                   return $ case m of
                                 Nothing -> Literal ch
                                 Just c -> Repetition $ Literal ch

groupParser :: GenParser Char st RegEx
groupParser = do r <- between (char '(') (char ')') regex
                 m <- optionMaybe $ char '*'
                 return $ case m of
                               Nothing -> r
                               Just c -> Repetition r

regexG :: GenParser Char st RegEx
regexG = do l1s <- try $ many literalParser
            grs <- try $ many groupParser
            l2s <- try $ many literalParser
            return $ Concatenation $ l1s ++ grs ++ l2s

unionParser :: GenParser Char st RegEx
unionParser = do char '|'
                 regexG

regex :: GenParser Char st RegEx
regex = do g <- regexG
           uns <- many unionParser
           return $ Union $ g : uns

parseStr :: String -> Either ParseError RegEx
parseStr = parse regex "(unknown)"

someFunc :: IO ()
someFunc = do putStrLn "Parser waiting: "
              str <- getLine
              let re = case parseStr str of
                      Left pe -> Literal 'y'
                      Right re -> re
              print re
              print $ buildNFA re

data Move a = Emove a a
            | Move a Char a
            deriving (Eq, Show, Ord)

incMove :: Move Int -> Int -> Move Int
incMove (Emove j k) l = Emove (j + l) (k + l)
incMove (Move j ch k) l = Move (j + l) ch (k + l)

data NFA a = NFA { states     :: Set a        -- Set of possible states
                 , startState :: a            -- start state
                 , moves      :: Set (Move a) -- set of possible moves
                 , accStates  :: Set a        -- set of acceptable states
                 } deriving (Eq, Show)

size :: NFA Int -> Int
size = S.size . states


buildNFA :: RegEx -> NFA Int
buildNFA (Literal ch)
  = NFA (fromList [0..1])
         0
        (singleton $ Move 0 ch 1)
        (singleton 1)
buildNFA (Concatenation l)
  = NFA (unions newStates)
         0
        (unions newMoves)
         finalState
    where
      nfal = P.map buildNFA l
      sizesl = P.map Lib.size nfal
      indicesl = scanl (\acc k -> acc + k - 1) 0 sizesl
      finalState = singleton (last indicesl)

      newStates = zipWith (\s k -> S.map (+k) s) (P.map states nfal) indicesl
      newMoves = zipWith (\ms k -> S.map (`incMove` k) ms) (P.map moves nfal) indicesl
buildNFA (Union l)
  = NFA (unions changedStates `union` newstates)
         0
        (unions finalMoves `union` newMovesFromSS `union` newMovesToES)
        (singleton finalSize)
    where
      nfal = P.map buildNFA l
      sizesl = P.map Lib.size nfal
      bindicesl = scanl (+) 1 sizesl
      eindicesl = tail $ scanl (+) 0 sizesl
      finalSize = last bindicesl

      newstates = fromList [0, finalSize]
      changedStates = zipWith (\s k -> S.map (+k) s) (P.map states nfal) bindicesl

      newMovesFromSS = fromList $ P.map (Emove 0) (init bindicesl)
      newMovesToES = fromList $ P.map (`Emove` finalSize) eindicesl
      finalMoves = zipWith (\ms k -> S.map (`incMove` k) ms) (P.map moves nfal) bindicesl
buildNFA (Repetition re)
  = NFA (changedStates `union` newStates)
         0
        (changedMoves `union` newMoves)
        (singleton latestState)
    where
      nfa = buildNFA re
      latestState = Lib.size nfa + 2
      changedStates = S.map (+1) (states nfa)
      newStates = fromList [0, latestState]
      changedMoves = S.map (`incMove` 1) (moves nfa)
      newMoves = fromList [Emove 0 latestState, Emove (latestState - 1) 1]

type family F a where
  F Int = Bool
  F Char = Double

useF :: F Int -> F Char
useF True = 1.0
