{-# LANGUAGE DeriveDataTypeable,RecordWildCards #-}
module CmdArgs(MateArgs(..), getMateArgs) where

import System.Console.CmdArgs
import Data.Maybe

data MateArgsMaybe = MateArgsMaybe {
     muidgid :: Maybe String
    ,mport :: Maybe Int
    ,mip :: Maybe String
    ,mmateport :: Maybe Int
} deriving (Data, Typeable)

mateArgs = MateArgsMaybe {
     muidgid = def &= name "d" &= help "drop privileges to \"user,group\" id, e.g. -dnobody,nogroup"
    ,mport = def &= name "p" &= help "bind to this port (web)"
    ,mip = def &= name "i" &= help "bind to this ip"
    ,mmateport = def &= name "m" &= help "bind to this port (crap)"
}

data MateArgs = MateArgs {
     uidgid :: Maybe (String, String)
    ,port :: Int
    ,ip :: String
    ,mateport :: Int
} deriving Show

insertDefaults :: MateArgsMaybe -> MateArgs
insertDefaults (MateArgsMaybe {..}) = MateArgs uidgid (fromMaybe 8080 mport) (fromMaybe "127.0.0.1" mip) (fromMaybe 1337 mmateport)
    where
    uidgid = muidgid >>= \uidgid' -> let (uid, ',':gid) = span (/=',') uidgid' in return (uid, gid)

mate = cmdArgsMode $ modes [mateArgs &= auto] &= help "Matelight Emulator" &= program "MateEmu" &= summary "A Matelight in your browser"

getMateArgs :: IO MateArgs
getMateArgs = fmap insertDefaults $ cmdArgsRun mate
