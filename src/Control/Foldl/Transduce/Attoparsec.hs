module Control.Foldl.Transduce.Attoparsec (
        stripParse
    ,   parses
    ,   ParserInput
    ,   ParsingError
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Attoparsec.Types
import Data.Attoparsec.ByteString
import Data.Attoparsec.Text
import qualified Data.Monoid.Null as MN
import qualified Data.Monoid.Factorial as MF
import Control.Monad.Trans.Except

import Control.Foldl.Transduce

{- $setup

>>> import qualified Control.Foldl as L
>>> import qualified Data.Text as T
>>> import Control.Foldl.Transduce
>>> import Control.Foldl.Transduce.Attoparsec
>>> import qualified Data.Attoparsec.Text as A
>>> import Control.Applicative
>>> import Control.Monad.Trans.Except

-}

data StripState a b 
    = Parsed b
    | Continue (a -> IResult a b)

{-| Strips from the stream the prefix that matches the parser.		

    The parsed value becomes the return value of the 'TransducerM'.


>>> runExcept $ L.foldM (transduceM (stripParse (many (A.char 'a'))) (L.generalize L.mconcat)) (map T.pack ["aa","aa","bb"])
Right "bb"

>>> runExcept $ L.foldM (transduceM' (stripParse (many (A.char 'a'))) (L.generalize L.mconcat)) (map T.pack ["aa","aa","bb"])
Right ("aaaa","bb")

>>> runExcept $ L.foldM (transduceM' (stripParse (A.char 'z')) (L.generalize L.mconcat)) (map T.pack ["aa","aa","bb"])
Left ("aa",["'z'"],"Failed reading: satisfy")
-}
stripParse :: (ParserInput a,Monad m) 
           => Data.Attoparsec.Types.Parser a b
           -> TransducerM (ExceptT (ParsingError a) m) a a b 
stripParse atto = TransducerM step (initial (_parse atto mempty)) done
    where
    initial iresult = case iresult of
        Fail l es e -> throwE (l,es,e)
        Done _ r -> return (Parsed r) -- there can't be leftovers here!
        Partial c -> return (Continue c)

    step x i 
        | MN.null i =
            return (x,[],[])
        | otherwise = case x of
            Parsed _ -> 
                return (x,[i],[])
            Continue c -> 
                case c i of
                    Fail l es e -> throwE (l,es,e)
                    Done l r -> return (Parsed r,[l],[]) 
                    Partial c' -> return (Continue c',[],[])

    done x = case x of
        Parsed r -> return (r,[],[])
        Continue c -> case c mempty of
            Fail l es e -> throwE (l,es,e)
            Done l r -> return (r,[l],[]) 
            Partial _ -> error "never happens"


data ParsesState a b 
    = BetweenParses
    | DuringParse (a -> IResult a b)

{-| Feeds a 'FoldM' with the results of repeatedly applying a parser.		


>>> runExcept $ L.foldM (transduceM (parses (A.decimal <* A.char ' ')) (L.generalize L.list)) (map T.pack ["1 1","1 3 77 "])
Right [1,11,3,77]

-}
parses :: (ParserInput a,Monad m) 
       => Data.Attoparsec.Types.Parser a b
       -> TransducerM (ExceptT (ParsingError a) m) a b ()
parses atto = TransducerM step (return BetweenParses) done
    where 
    step x i = do
        (x',rs) <- step' [] x i
        return (x',reverse rs,[])
        
    step' acc xx i 
        | MN.null i =
            return (xx,acc)
        | otherwise =
           case xx of
               BetweenParses -> case _parse atto mempty of
                   Fail l es e -> throwE (l,es,e)
                   Done _ _ -> throwE (mempty,[],"Parser doesn't consume!")
                   Partial c -> step' acc (DuringParse c) i

               DuringParse c -> case c i of 
                   Fail l es e -> throwE (l,es,e)
                   Done l r -> step' (r:acc) BetweenParses l
                   Partial c' -> return (DuringParse c',acc)

    done x = case x of
        BetweenParses -> return mempty
        DuringParse c -> case c mempty of 
            Fail l es e -> throwE (l,es,e)
            Done _ r -> return ((),[r],[]) -- there shouldn't be leftovers, I think.
            Partial _ -> error "never happens"

-- From the attoparsec docs:
-- http://hackage.haskell.org/package/attoparsec-0.13.0.1/docs/Data-Attoparsec-Text.html
-- "To indicate that you have no more input, supply the Partial continuation with an empty Text."

class (Eq a,MF.StableFactorialMonoid a) => ParserInput a where
    _parse :: Data.Attoparsec.Types.Parser a b -> a -> IResult a b

instance ParserInput B.ByteString where
    _parse  = Data.Attoparsec.ByteString.parse
    {-# INLINE _parse #-}

-- | Strict 'Text'.
instance ParserInput T.Text where
    _parse  = Data.Attoparsec.Text.parse
    {-# INLINE _parse #-}

{-| A triplet of (leftovers,error contexts,error message)

-}
type ParsingError a = (a,[String],String)

