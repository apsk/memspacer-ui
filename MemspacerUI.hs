{-# LANGUAGE TypeFamilies, GADTs, FlexibleContexts, LambdaCase, RecordWildCards,
             QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module MemspacerUI where

import Data.Char
import Data.List (intercalate)
import Data.Time
import Data.Default
import Data.IORef

import Text.Read

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad
import Control.Lens

import Database.Persist as P
import Database.Persist.Sqlite as P
import Database.Persist.TH as P

import Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade as G

import System.Exit

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

cmdArgs Config{..} = intercalate " "
  [ if configMode == 0 then "-dm" else "-sm"
  , "-n=" ++ show configBuffLen
  , if configUseZRot then "-z" else ""
  , if configUseColor then "-c" else ""
  , if configUseSound then "-s" else ""
  , if configUseSpaceBg then "-bg" else ""
  , "-ic=" ++ show configIdleColor
  , "-bc=" ++ show configBlinkColor
  , "-bt=" ++ show configBlinkTime
  , "-it=" ++ show configInterval ]

activeProfile =
  selectFirst [GlobalKey ==. "active_profile"] [] >>= \case
    Just (Entity _ (Global _ profile)) -> return profile
    Nothing -> do
      insert $ Global "active_profile" "default"
      return "default"

configFor profile =
  selectFirst [ConfigProfile ==. profile] [] >>= \case
    Just (Entity key cfg) -> return (key, cfg)
    Nothing -> do
      let cfg :: ConfigGeneric backend
          cfg = def { configProfile = profile }
      (,) <$> insert cfg <*> pure cfg

db = runSqlite "mspc.db"

whenM mb action = mb >>= \b -> when b action

toolButtonClicked = Signal $ \_ -> onToolButtonClicked

wire widget signal action = on widget signal (action widget)

widgets ui cast = mapM (xmlGetWidget ui cast)

wireUI ui initialConfigKey initialConfig = do
  [defaultModeRB, shiftingModeRB] <-
    widgets ui castToRadioButton ["defaultModeRB", "shiftingModeRB"]
  [buffLenE, blinkTimeE, intervalE] <-
    widgets ui castToEntry ["buffLenE", "blinkTimeE", "intervalE"]
  [zRotCB, colorCB, soundCB, spaceCB] <-
    widgets ui castToCheckButton ["zRotCB", "colorCB", "soundCB", "spaceCB"]
  [addTB, saveTB, revertTB] <- widgets ui castToToolButton ["addTB", "saveTB", "revertTB"]
  [runB, exitB] <- widgets ui castToButton ["runB", "exitB"]
  configKeyRef <- newIORef initialConfigKey
  configRef    <- newIORef initialConfig
  let getConfig :: Getting a (ConfigGeneric SqlBackend) a -> IO a
      getConfig lens = (^. lens) <$> readIORef configRef
  let setConfig = modifyIORef configRef
  let resetUI = do
        toggleButtonSetActive defaultModeRB  =<< (== 0) <$> getConfig mode
        toggleButtonSetActive shiftingModeRB =<< (== 1) <$> getConfig mode
        entrySetText buffLenE =<< show <$> getConfig buffLen
        toggleButtonSetActive zRotCB  =<< getConfig useZRot
        toggleButtonSetActive colorCB =<< getConfig useColor
        toggleButtonSetActive soundCB =<< getConfig useSound
        toggleButtonSetActive spaceCB =<< getConfig useSpaceBg
        entrySetText blinkTimeE =<< show <$> getConfig blinkTime
        entrySetText intervalE  =<< show <$> getConfig interval
  resetUI
  wire defaultModeRB toggled $
    \b -> whenM (toggleButtonGetActive b) $ setConfig (mode .~ 0)
  wire shiftingModeRB toggled $
    \b -> whenM (toggleButtonGetActive b) $ setConfig (mode .~ 1)
  wire buffLenE editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (buffLen .~ maybe 1 id (readMaybe val))
  wire zRotCB toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useZRot .~ tog)
  wire colorCB toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useColor .~ tog)
  wire soundCB toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useSound .~ tog)
  wire spaceCB toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useSpaceBg .~ tog)
  wire blinkTimeE editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (blinkTime .~ maybe 1 id (readMaybe val))
  wire intervalE editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (interval .~ maybe 1 id (readMaybe val))
  wire saveTB toolButtonClicked $
    \_ -> db =<< replace <$> readIORef configKeyRef <*> readIORef configRef
  wire revertTB toolButtonClicked $
    \_ -> (writeIORef configRef =<< snd <$> db (configFor =<< activeProfile)) >> resetUI
  wire runB buttonActivated $
    \_ -> putStrLn =<< cmdArgs <$> readIORef configRef
  wire exitB buttonActivated $
    \_ -> exitSuccess

main = do
  (initialConfigKey, initialConfig) <- db $ do
    runMigration migrateAll
    configFor =<< activeProfile
  initGUI
  Just ui <- xmlNew "ui.glade"
  wnd <- xmlGetWidget ui castToWindow "wnd"
  onDestroy wnd mainQuit
  wireUI ui initialConfigKey initialConfig
  widgetShowAll wnd
  mainGUI
