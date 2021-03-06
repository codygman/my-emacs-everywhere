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

main :: IO ()
main = do
  userHome <- home
  doomDir  <- pwd
  let doomDirNixpkgs   = doomDir </> decodeString "nixpkgs"
  let nixpkgConfigPath = userHome </> decodeString ".config/nixpkgs"
  let bootstrapBackups = userHome </> decodeString ".bootstrap-backups"
  maybeBackupNixpkgConfig nixpkgConfigPath bootstrapBackups
    -- symlink file (or copy on android, then rename home.nix to nix-on-droid.nix)
  host <- hostname
  case host of
    "localhost" -> do
      echo "android detected, using copying instead of symlinks"
      cptree doomDirNixpkgs nixpkgConfigPath
      testdir nixpkgConfigPath >>= \there -> do
        if there
          then mv (nixpkgConfigPath </> decodeString "nix-on-droid.nix")
                  (nixpkgConfigPath </> decodeString "nix-on-droid.nix")
          else pure ()
    "nixos" -> symlink doomDirNixpkgs nixpkgConfigPath
    unknown -> if T.isPrefixOf "travis-job" unknown
      then symlink doomDirNixpkgs nixpkgConfigPath
      else error $ "unkown system, not sure what to do: " <> show unknown
  dirsExist <- sequenceA [testdir doomDirNixpkgs, testdir nixpkgConfigPath]
  if all (== True) dirsExist then pure () else exit (ExitFailure 1)
  view $ proc
    "nix-channel"
    [ "--add"
    , "https://github.com/rycee/home-manager/archive/master.tar.gz"
    , "home-manager"
    ]
    empty
  case host of
    "localhost" -> do
      echo "cannot install nix-on-droid automatically (yet)"
      which "nix-on-droid" >>= \hm -> case hm of
        Just _  -> view $ shell "nix-on-droid switch" empty
        Nothing -> error "install nix-on-droid manually"
    "nixos" -> do
      which "home-manager" >>= \hm -> case hm of
        Just _  -> view $ shell "home-manager switch" empty
        Nothing -> do
          -- WIP get rid of leading "Shell: " part of text values
          proc "nix-channel" ["--update"] empty
          view $ shell "nix-shell '<home-manager>' -A install" empty

maybeBackupNixpkgConfig nixpkgConfigPath bootstrapBackups = do
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

          testdir nixpkgConfigPath >>= \there -> do
            if there then mv nixpkgConfigPath backupPath else pure ()

echoTxt :: Text -> IO ()
echoTxt = echo . unsafeTextToLine
