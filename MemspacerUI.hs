{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, FlexibleInstances,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, StandaloneDeriving,
             GADTs, TypeFamilies, FlexibleContexts, PackageImports,
             LambdaCase, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module MemspacerUI where

import Data.Char
import Data.Time
import Data.Default
import Data.IORef

import Text.Read

import Control.Applicative
import Control.Monad
import Control.Lens

import Database.Persist as P
import Database.Persist.Sqlite as P
import Database.Persist.TH as P

import Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade as G

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll" ]
  [persistLowerCase|
    Global
      key String
      val String
    Config
      profile    String
      mode       Int
      buffLen    Int
      useZRot    Bool
      useColor   Bool
      useSound   Bool
      useSpaceBg Bool
      idleColor  Int
      blinkColor Int
      blinkTime  Int
      interval   Int
      deriving Show
    Session
      pid  ConfigId
      date UTCTime
      deriving Show
  |]

makeLensesFor (map (\nm@(c:cs) -> ("config" ++ toUpper c : cs, nm))
                 [ "profile", "mode", "buffLen", "useZRot"
                 , "useColor", "useSound", "useSpaceBg"
                 , "idleColor" , "blinkColor", "blinkTime", "interval" ])
  ''ConfigGeneric

instance Default (ConfigGeneric b) where
  def = Config "default" 0 1 False False False False 0 0 1 3

activeProfile =
  selectFirst [GlobalKey ==. "active_profile"] [] >>= \case
    Just (Entity _ (Global _ profile)) -> return profile
    Nothing -> do
      insert $ Global "active_profile" "default"
      return "default"

configFor profile =
  selectFirst [ConfigProfile ==. profile] [] >>= \case
    Just (Entity _ cfg) -> return cfg
    Nothing -> do
      let cfg :: ConfigGeneric backend
          cfg = def { configProfile = profile }
      insert cfg
      return cfg

wire_ ui cast name signal action = do
  widget <- xmlGetWidget ui cast name
  on widget signal action

wire ui cast name signal action = do
  widget <- xmlGetWidget ui cast name
  on widget signal (action widget)

whenM mb action = mb >>= \b -> when b action

wireUI ui configRef = do
  let configure = modifyIORef configRef
  wire ui castToRadioButton "defaultModeRB" toggled $ \b ->
    whenM (toggleButtonGetActive b) $ configure (mode .~ 0)
  wire ui castToRadioButton "shiftingModeRB" toggled $ \b ->
    whenM (toggleButtonGetActive b) $ configure (mode .~ 1)
  wire ui castToEntry "buffLenE" editableChanged $ \e -> do
    val <- entryGetText e
    configure (buffLen .~ maybe 1 id (readMaybe val))
  wire ui castToCheckButton "zRotCB" toggled $ \b ->
    toggleButtonGetActive b >>= \tog -> configure (useZRot .~ tog)
  wire ui castToCheckButton "colorCB" toggled $ \b ->
    toggleButtonGetActive b >>= \tog -> configure (useColor .~ tog)
  wire ui castToCheckButton "soundCB" toggled $ \b ->
    toggleButtonGetActive b >>= \tog -> configure (useSound .~ tog)
  wire ui castToCheckButton "spaceCB" toggled $ \b ->
    toggleButtonGetActive b >>= \tog -> configure (useSpaceBg .~ tog)
  wire ui castToEntry "blinkTimeE" editableChanged $ \e -> do
    entryGetText e >>= \val -> configure (blinkTime .~ maybe 1 id (readMaybe val))
  wire ui castToEntry "intervalE" editableChanged $ \e -> do
    entryGetText e >>= \val -> configure (interval .~ maybe 1 id (readMaybe val))

main = do
  config <- runSqlite "mspc.db" $ do
    runMigration migrateAll
    profile <- activeProfile
    configFor profile
  configRef <- newIORef config
  initGUI
  Just ui <- xmlNew "ui.glade"
  wnd <- xmlGetWidget ui castToWindow "wnd"
  onDestroy wnd mainQuit
  wireUI ui configRef
  widgetShowAll wnd
  mainGUI
