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
import Data.IORef
import Data.XML.Types


data Parser = Parser {
    parserErrorHandler :: String -> IO (),
    parserFilename :: Maybe String,
    parserInputBuffer :: IORef ByteString,
    parserBeginDocumentCallback
      :: IORef (Maybe (IO Bool)),
    parserEndDocumentCallback
      :: IORef (Maybe (IO Bool)),
    parserBeginElementCallback
      :: IORef (Maybe (Name -> [Attribute] -> IO Bool)),
    parserEndElementCallback
      :: IORef (Maybe (Name -> IO Bool)),
    parserCharactersCallback
      :: IORef (Maybe (String -> IO Bool)),
    parserCommentCallback
      :: IORef (Maybe (String -> IO Bool)),
    parserInstructionCallback
      :: IORef (Maybe (Instruction -> IO Bool)),
    parserDoctypeCallback
      :: IORef (Maybe (Doctype -> IO Bool))
  }


data Callback a where
  CallbackBeginDocument :: Callback (IO Bool)
  CallbackEndDocument :: Callback (IO Bool)
  CallbackBeginElement :: Callback (Name -> [Attribute] -> IO Bool)
  CallbackEndElement :: Callback (Name -> IO Bool)
  CallbackCharacters :: Callback (String -> IO Bool)
  CallbackComment :: Callback (String -> IO Bool)
  CallbackInstruction :: Callback (Instruction -> IO Bool)
  CallbackDoctype :: Callback (Doctype -> IO Bool)


newParser :: (String -> IO ())
          -> Maybe String
          -> IO Parser
newParser errorHandler maybeFilename = do
  inputBufferIORef <- newIORef BS.empty
  beginDocumentCallbackIORef <- newIORef Nothing
  endDocumentCallbackIORef <- newIORef Nothing
  beginElementCallbackIORef <- newIORef Nothing
  endElementCallbackIORef <- newIORef Nothing
  charactersCallbackIORef <- newIORef Nothing
  commentCallbackIORef <- newIORef Nothing
  instructionCallbackIORef <- newIORef Nothing
  doctypeCallbackIORef <- newIORef Nothing
  return Parser {
             parserErrorHandler = errorHandler,
             parserFilename = maybeFilename,
             parserInputBuffer = inputBufferIORef,
             parserBeginDocumentCallback = beginDocumentCallbackIORef,
             parserEndDocumentCallback = endDocumentCallbackIORef,
             parserBeginElementCallback = beginElementCallbackIORef,
             parserEndElementCallback = endElementCallbackIORef,
             parserCharactersCallback = charactersCallbackIORef,
             parserCommentCallback = commentCallbackIORef,
             parserInstructionCallback = instructionCallbackIORef,
             parserDoctypeCallback = doctypeCallbackIORef
           }


setCallback :: Parser -> Callback a -> a -> IO ()
setCallback parser which callback = do
  case which of
    CallbackBeginDocument -> do
      writeIORef (parserBeginDocumentCallback parser) $ Just callback
    CallbackEndDocument -> do
      writeIORef (parserEndDocumentCallback parser) $ Just callback
    CallbackBeginElement -> do
      writeIORef (parserBeginElementCallback parser) $ Just callback
    CallbackEndElement -> do
      writeIORef (parserEndElementCallback parser) $ Just callback
    CallbackCharacters -> do
      writeIORef (parserCharactersCallback parser) $ Just callback
    CallbackComment -> do
      writeIORef (parserCommentCallback parser) $ Just callback
    CallbackInstruction -> do
      writeIORef (parserInstructionCallback parser) $ Just callback
    CallbackDoctype -> do
      writeIORef (parserDoctypeCallback parser) $ Just callback


clearCallback :: Parser -> Callback a -> IO ()
clearCallback parser which = do
  case which of
    CallbackBeginDocument -> do
      writeIORef (parserBeginDocumentCallback parser) Nothing
    CallbackEndDocument -> do
      writeIORef (parserEndDocumentCallback parser) Nothing
    CallbackBeginElement -> do
      writeIORef (parserBeginElementCallback parser) Nothing
    CallbackEndElement -> do
      writeIORef (parserEndElementCallback parser) Nothing
    CallbackCharacters -> do
      writeIORef (parserCharactersCallback parser) Nothing
    CallbackComment -> do
      writeIORef (parserCommentCallback parser) Nothing
    CallbackInstruction -> do
      writeIORef (parserInstructionCallback parser) Nothing
    CallbackDoctype -> do
      writeIORef (parserDoctypeCallback parser) Nothing


parsedBeginDocument :: Callback (IO Bool)
parsedBeginDocument = CallbackBeginDocument


parsedEndDocument :: Callback (IO Bool)
parsedEndDocument = CallbackEndDocument


parsedBeginElement :: Callback (Name -> [Attribute] -> IO Bool)
parsedBeginElement = CallbackBeginElement


parsedEndElement :: Callback (Name -> IO Bool)
parsedEndElement = CallbackEndElement


parsedCharacters :: Callback (String -> IO Bool)
parsedCharacters = CallbackCharacters


parsedComment :: Callback (String -> IO Bool)
parsedComment = CallbackComment


parsedInstruction :: Callback (Instruction -> IO Bool)
parsedInstruction = CallbackInstruction


parsedDoctype :: Callback (Doctype -> IO Bool)
parsedDoctype = CallbackDoctype


parseBytes :: Parser -> ByteString -> IO ()
parseBytes parser bytes = do
  undefined


parseComplete :: Parser -> IO ()
parseComplete parser = do
  undefined
