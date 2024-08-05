module Editor.Hint where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, displayException)
import Control.Monad.Catch (catch)
import Data.IORef
import Data.List (intercalate)
import Language.Haskell.Interpreter as Hint
import Sound.Tidal.Context (Stream)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

data InterpreterMessage
  = MStat String
  | MType String
  | MLoad String
  deriving (Show)

data InterpreterResponse
  = RStat (Maybe String)
  | RType String
  | RError String
  deriving (Show)

extensions :: [Hint.Extension]
extensions = [OverloadedStrings, NoMonomorphismRestriction]

hintJob :: Stream -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob str mMV rMV = do
  result <-
    catch
      (Hint.runInterpreter $ staticInterpreter str >> interpreterLoop mMV rMV)
      (\e -> return (Left $ UnknownError $ show (e :: SomeException)))
  -- can this happen? If it happens all definitions made interactively are lost...
  let response = case result of
        Left err -> RError (displayException err)
        Right p -> RError (show p)
  putMVar rMV response
  hintJob str mMV rMV

-- this is the basic interpreter that will be only loaded once
staticInterpreter :: Stream -> Interpreter ()
staticInterpreter str = do
  Hint.set [languageExtensions := extensions]
  Hint.setImportsF [ModuleImport x NotQualified NoImportList | x <- ["Sound.Tidal.Context", "Data.IORef", "Prelude"]]
  bind "tidal" str
  -- Hint.runStmt bootTidal
  return ()

-- this is the intrepreter receiving and interpreteing messages and sending the results back
interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
  message <- liftIO $ takeMVar mMV
  case message of
    MStat cont -> catch (interpretStatement cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ displayException (e :: SomeException))
    MType cont -> catch (interpretType cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ displayException (e :: SomeException))
    MLoad path -> return () -- catch (interpretFile path rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
  interpreterLoop mMV rMV

interpretStatement :: String -> MVar InterpreterResponse -> Interpreter ()
interpretStatement cont rMV = do
  t <- Hint.typeChecksWithDetails cont
  case t of
    -- if the expression doesn't type check try to just evaluate it (it could be a definition or binding)
    Left _ ->
      catch
        (Hint.runStmt cont >> liftIO (putMVar rMV $ RStat Nothing))
        (\e -> liftIO $ putMVar rMV $ RError $ displayException (e :: SomeException))
    Right _ -> do
      Hint.runStmt ("temp <- " ++ cont)
      out <- Hint.eval "temp"
      case out of
        "()" -> liftIO (putMVar rMV $ RStat Nothing)
        _ -> liftIO $ putMVar rMV $ RStat (Just out)

interpretType :: String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont rMV = do
  t <- Hint.typeChecksWithDetails cont
  case t of
    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
    Right out -> liftIO $ putMVar rMV $ RType out

-- interpretFile :: String -> MVar InterpreterResponse -> Interpreter ()
-- interpretFile path rMV = do
--   cont <- liftIO $ readFile path
--   let bs = blocks cont
--   catch ((sequence $ map Hint.runStmt bs) >> (liftIO $ putMVar rMV $ RStat Nothing) >> return ()) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)

bind :: String -> Stream -> Interpreter ()
bind var value = do
  Hint.runStmt "tmpIORef <- newIORef (undefined :: Stream)"
  tmpIORef <- Hint.interpret "tmpIORef" (Hint.as :: IORef Stream)
  liftIO $ writeIORef tmpIORef value
  Hint.runStmt (var ++ " <- readIORef tmpIORef")

runManyStmt :: [String] -> Interpreter ()
runManyStmt [] = return ()
runManyStmt (x : xs) = do
  runStmt x
  runManyStmt xs
