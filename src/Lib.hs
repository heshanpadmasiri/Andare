{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc,
      readConfig,
      Quest (..)
    ) where
import TOML (DecodeTOML, tomlDecoder, getField, decodeFile, Decoder, TOMLError)
import GHC.RTS.Flags ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- TODO: we need an array of things
-- TODO: change the toml and configuration to match an actual quest whith place holder text
data Quest = Quest {
   title:: String
} deriving (Show, Eq)

-- data SubQuest = SubQuest {
--     thingTitle :: String,
--     thingField :: String
-- } deriving (Show)

instance DecodeTOML Quest where
    tomlDecoder :: Decoder Quest
    tomlDecoder = 
        Quest
            <$> getField "title"

readConfig:: FilePath -> IO (Either Quest TOMLError)
readConfig path = do
    result <- decodeFile path
    case result of
        Right cfg -> return (Left (cfg :: Quest))
        Left e -> return (Right e)
