{-# LANGUAGE GADTs #-}
module Text.XML.Direct.SAX (
                            Parser,
                            newParser,
                            Callback,
                            setCallback,
                            clearCallback,
                            parsedBeginDocument,
                            parsedEndDocument,
                            parsedBeginElement,
                            parsedEndElement,
                            parsedCharacters,
                            parsedComment,
                            parsedInstruction,
                            parsedDoctype,
                            parseBytes,
                            parseComplete
                           )
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.XML.Types


data Parser = Parser {
    
  }


data Callback a where
  CallbackBeginDocument :: Callback (IO Bool)
  CallbackEndDocument :: Callback (IO Bool)


newParser :: (String -> IO ())
          -> Maybe String
          -> IO Parser
newParser errorHandler maybeFilename = do
  return undefined


setCallback :: Parser -> Callback a -> a -> IO ()
setCallback parser which callback = do
  undefined


clearCallback :: Parser -> Callback a -> IO ()
clearCallback parser which = do
  undefined


parsedBeginDocument :: Callback (IO Bool)
parsedBeginDocument = undefined


parsedEndDocument :: Callback (IO Bool)
parsedEndDocument = undefined


parsedBeginElement :: Callback (Name -> [Attribute] -> IO Bool)
parsedBeginElement = undefined


parsedEndElement :: Callback (Name -> IO Bool)
parsedEndElement = undefined


parsedCharacters :: Callback (String -> IO Bool)
parsedCharacters = undefined


parsedComment :: Callback (String -> IO Bool)
parsedComment = undefined


parsedInstruction :: Callback (Instruction -> IO Bool)
parsedInstruction = undefined


parsedDoctype :: Callback (Doctype -> IO Bool)
parsedDoctype = undefined


parseBytes :: Parser -> ByteString -> IO ()
parseBytes parser bytes = do
  undefined


parseComplete :: Parser -> IO ()
parseComplete parser = do
  undefined
