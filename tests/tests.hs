{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.Bifunctor
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Control.Foldl.Transduce.Attoparsec



main :: IO ()
main = defaultMain tests

stripping :: A.Parser r -> [T.Text] -> Either (ParsingError T.Text) (r,T.Text)
stripping parser ts = 
     runExcept (L.foldM (transduceM' (stripParse parser) (L.generalize L.mconcat)) ts)

parsing :: A.Parser r -> [T.Text] -> Either (ParsingError T.Text) [r]
parsing parser ts = 
     runExcept (fmap snd (L.foldM (transduceM' (parses parser) (L.generalize L.list)) ts))

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        testGroup "stripPrefix" 
        [ testCase "empty"  
              (assertEqual mempty
                  (Right ((),""))
                  (stripping (pure ()) [""]))
        , testCase "empty2"  
              (assertEqual mempty
                  (Right ([],""))
                  (stripping (many (A.char 'c')) [""]))
        , testCase "empty3"  
              (assertBool mempty
                  (isLeft (stripping (A.char 'c') [""])))
        , testCase "acrossChunks"  
              (assertEqual mempty
                  (Right (1111::Int,"aaabb"))
                  (stripping (A.skipSpace *> A.decimal) [" "," 11", "11aaa", "bb"]))
        , testCase "whole"  
              (assertEqual mempty
                  (Right ("cccc",""))
                  (stripping (many (A.char 'c')) ["cc","","cc"]))
        , testCase "atEnd"  
              (assertEqual mempty
                  (Right (1111::Int,""))
                  (stripping (A.skipSpace *> A.decimal) [" "," 11", "", "11"]))
        ]   
    ,   testGroup "parses" 
        [ testCase "empty"  
              (assertEqual mempty
                  (Right ([]::[Int]))
                  (parsing (A.skipSpace *> A.decimal) []))
        , testCase "chunks"  
              (assertEqual mempty
                  (Right ([1,22,3,4,5]::[Int]))
                  (parsing (A.skipSpace *> A.decimal) [""," ","1 2","","2",""," 3 4 5"]))
        , testCase "whole"  
              (assertEqual mempty
                  (Right ([1111]::[Int]))
                  (parsing (A.skipSpace *> A.decimal) [" 1111"]))
        ]   
    ]
