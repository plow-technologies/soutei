-- helpers for Parsec

module Soutei.Parsec where

import           Control.Monad.Identity
import           Text.ParserCombinators.Parsec

junk :: Monad m => m a -> m ()
junk p = p >> return ()

parseM :: Monad m => Parser a -> FilePath -> String -> m a
parseM p f s = handleM (parse p f s)

parseFromFileM :: Parser a -> FilePath -> IO a
parseFromFileM p f = parseFromFile p f >>= handleM

handleM (Left e)  = fail (show e)
handleM (Right x) = return x

uncheckedParse :: Parser a -> String -> a
uncheckedParse p s = runIdentity (parseM p "string" s)
