#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.turtle])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/d5291756487d70bc336e33512a9baf9fa1788faf.tar.gz
{-# LANGUAGE OverloadedStrings #-}
-- NOTE: nixpkgs is pinned to nixpkgs-1909 2020-03-28
-- it's 1909 mostly so that I don't have to build ghc on android
-- which kept failing for some reason with previous pin
import qualified Data.Text                     as T
import           Turtle
import           Data.Time
import           Control.Monad

main = do
  userHome <- home
  doomDir  <- pwd
  let doomDirNixpkgs   = doomDir </> decodeString "nixpkgs"
      nixpkgConfigPath = userHome </> decodeString ".config/nixpkgs"
  initialSetup doomDirNixpkgs nixpkgConfigPath
  view $ homeManager ["switch"]

initialSetup doomDirNixpkgs nixpkgConfigPath = do
    testdir nixpkgConfigPath >>= \there -> do
      if there then
        pure ()
      else
        symlink doomDirNixpkgs nixpkgConfigPath
    which "home-manager" >>= \hm -> case hm of
      Just _ -> pure ()
      Nothing -> installHomeManager
    doomSetup
  

doomInstalled :: IO Bool
doomInstalled = pure False

configDoomExists :: IO Bool
configDoomExists = do
  userHome <- home
  echo "A ~/.config/doom folder exists, assuming it's the right one"
  testdir (userHome </> ".config" </> "doom")

emacsDExists = do
  userHome <- home
  echo "An emacs.d folder exists, assuming it's the right one"
  testdir (userHome </> ".emacs.d")

cloneDoomEmacsD = do
   userHome <- home
   testdir (userHome </> ".emacs.d") >>= \there -> if there then empty else
     git ["clone","-b","develop", "git://github.com/hlissner/doom-emacs", format fp (userHome </> ".emacs.d")]

copyConfigDoom = pure ()

doomSetup = do
  doomInstalled >>= \di -> when (not di) $ do
    echo "doom setup"
    view $ cloneDoomEmacsD
  configDoomExists >>= \cde -> when (not cde) $ copyConfigDoom
  emacsDExists >>= \ede -> when (not ede) $ do
    echo "Please run doom setup command manually:"
    echo "~/.emacs.d/bin/doom install"

installHomeManager = do
  view $ proc
      "nix-channel"
      [ "--add"
      , "https://github.com/rycee/home-manager/archive/master.tar.gz"
      , "home-manager"
      ]
    empty
  view $ proc "nix-channel" ["--update"] empty
  view $ shell "nix-shell '<home-manager>' -A install" empty

homeManager opts = inproc "home-manager" opts empty

git opts = inproc "git" opts empty

doom opts = home >>= \uh -> inproc (format fp (uh </> ".emacs.d" </> "bin" </> "doom")) opts empty

echoTxt :: Text -> IO ()
echoTxt = echo . unsafeTextToLine