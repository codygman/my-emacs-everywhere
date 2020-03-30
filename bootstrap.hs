#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.turtle])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/d5291756487d70bc336e33512a9baf9fa1788faf.tar.gz
{-# LANGUAGE OverloadedStrings #-}
-- NOTE: nixpkgs is pinned to nixpkgs-1909 2020-03-28
-- it's 1909 mostly so that I don't have to build ghc on android
-- which kept failing for some reason with previous pin
import           Turtle
import           Data.Time
import           Control.Monad

main :: IO ()
main = do
  userHome <- home
  doomDir  <- pwd
  let doomDirNixpkgs   = doomDir </> decodeString "nixpkgs"
  let nixpkgConfigPath = userHome </> decodeString ".config/nixpkgs"
  let bootstrapBackups = userHome </> decodeString ".bootstrap-backups"
  testdir nixpkgConfigPath >>= \there -> do
    echoTxt $ format ("NOTE: moving current config to " % fp) bootstrapBackups
    -- create backup directory
    mktree bootstrapBackups
    -- move old folder with timestamp into backup directory
    date >>= \now ->
      let
        timestamp = fromString $ formatTime
          defaultTimeLocale
          (iso8601DateFormat $ Just "%H_%M_%S")
          now
        nixpkgsPrefix = ("nixpkgs_" :: Text)
        backupPath =
          bootstrapBackups </> fromText (format (s % s) nixpkgsPrefix timestamp)
      in
        do
           -- TODO don't want to do this every time unless there's a change
          echoTxt $ format ("moving '" % fp % "' to '" % fp % "'")
                           nixpkgConfigPath
                           backupPath
          mv nixpkgConfigPath backupPath
    -- symlink file (or copy on android, then rename home.nix to nix-on-droid.nix)
    hostname >>= \hn -> case hn of
      "localhost" -> do
        echo "android detected, using copying instead of symlinks"
        cp doomDirNixpkgs nixpkgConfigPath
        testdir nixpkgConfigPath >>= \there -> do
          mv (nixpkgConfigPath </> decodeString "home.nix")
             (nixpkgConfigPath </> decodeString "nix-on-droid.nix")
        echo "renamed home.nix to nix-on-droid.nix"
      "nixos" -> symlink doomDirNixpkgs nixpkgConfigPath
      unknown -> error $ "unkown system, not sure what to do: " <> show unknown
  dirsExist <- sequenceA [testdir doomDirNixpkgs, testdir nixpkgConfigPath]
  if all (== True) dirsExist then pure () else exit (ExitFailure 1)
  view $ proc
    "nix-channel"
    [ "--add"
    , "https://github.com/rycee/home-manager/archive/master.tar.gz"
    , "home-manager"
    ]
    empty
  which "home-manager" >>= \hm -> case hm of
    Just _  -> view $ shell "home-manager switch" empty
    Nothing -> do
      -- WIP get rid of leading "Shell: " part of text values
      proc "nix-channel" ["--update"] empty
      view $ shell "nix-shell '<home-manager>' -A install" empty

echoTxt :: Text -> IO ()
echoTxt = echo . unsafeTextToLine
