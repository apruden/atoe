{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module App where

import Web.Scotty

import Data.List hiding (delete)
import Data.Default (def)
import Data.Typeable
import qualified Data.Map as Map

-- Network
import Network.Wai.Middleware.Static
-- import Network.Wai.Handler.Warp (settingsPort)
import Network.HTTP.Types  (status404, status200)

-- Database
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Control.Monad.State as S
import Control.Lens (makeLenses)

import Control.Monad
import Language.Haskell.Interpreter hiding (get)


------------------------------------------------------------------------------

type Key = String
type Value = String

data Database = Database !(Map.Map Key Value)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''Database)
makeLenses ''Database

------------------------------------------------------

insertKey :: Key -> Value -> Update Database ()
insertKey key value
    = do Database m <- S.get
         S.put (Database (Map.insert key value m))

lookupKey :: Key -> Query Database (Maybe Value)
lookupKey key
    = do Database m <- ask
         return (Map.lookup key m)

deleteKey :: Key -> Update Database ()
deleteKey key
    = do Database m <- S.get
         S.put (Database (Map.delete key m))

allKeys :: Int -> Query Database [(Key, Value)]
allKeys limit
    = do Database m <- ask
         return $ take limit (M.toList m)

$(makeAcidic ''Database ['insertKey, 'lookupKey, 'allKeys, 'deleteKey])

------------------------------------------------------------------------------

fixtures :: M.Map String String
fixtures = Map.empty

------------------------------------------------------------------------------

run :: IO ()
run = do
    putStrLn "Loading Database."
    database <- openLocalStateFrom "db/" (Database fixtures)

    putStrLn "Starting HTTP Server."
    scotty 3000 $ do
        --middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ file "index.html"

        get "/tasks/" $ do
            result <- liftIO $ query database (AllKeys 10)
            json result

        get "/tasks/:key" $ do
            key <- param "key"
            result <- liftIO $ query database (LookupKey key)
            case result of
               Nothing    -> status status404
               Just value -> json value

        delete "/tasks/:key" $ do
            key <- param "key"
            _   <- liftIO $ update database (DeleteKey key)
            status status200

        post "/tasks/" $ do
            (key, val) <- jsonData
            _          <- liftIO $ update database (InsertKey key val)
            status status200

        post "/tasks/:key" $ do
            key <- param "key"
            result <- liftIO $ query database (LookupKey key)
            --liftIO $ do
            --            r <- runInterpreter testHint
            --            case r of
            --                Left err -> liftIO $ putStrLn (errorString err)
            --                Right () -> return ()
            case result of
               Nothing    -> status status404
               Just value -> do
                                r <- liftIO $ runInterpreter $ executeTask value
                                case r of
                                    Left err -> json $ errorString err
                                    Right val -> json val


errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

executeTask :: String -> Interpreter String
executeTask task =
    do
        setImportsQ [("Prelude", Nothing)]
        -- liftIO $ putStrLn (show (fun (Just 7)))
        output <- eval task
        return $ output
