module Control.Foldl.Transduce.Attoparsec (
        ParserInput
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text

import qualified Data.Monoid.Factorial as MF

import Data.Attoparsec.Types
import Data.Attoparsec.ByteString
import Data.Attoparsec.Text

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

--
----------------------------------------------------------------------------------
--
---- | A parsing error report, as provided by Attoparsec's 'Fail'.
--data ParsingError = ParsingError
--    { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
--    , peMessage  ::  String   -- ^ Parsing error description message.
--    } deriving (Show, Read, Eq, Data, Typeable)
--
--instance Exception ParsingError
--instance Error     ParsingError
--
---- | This instance allows using 'Pipes.Lift.errorP' with 'parsed' and 'parsedL'
--instance Error (ParsingError, Producer a m r)
--
----------------------------------------------------------------------------------
---- Internal stuff
--
---- | Like 'Pipes.next', except it skips leading 'mempty' chunks.
--nextSkipEmpty
--  :: (Monad m, Eq a, Monoid a)
--  => Producer a m r
--  -> m (Either r (a, Producer a m r))
--nextSkipEmpty = go where
--    go p0 = do
--      x <- next p0
--      case x of
--         Left  _        -> return x
--         Right (a,p1)
--          | a == mempty -> go p1
--          | otherwise   -> return x
--{-# INLINABLE nextSkipEmpty #-}
--
--
