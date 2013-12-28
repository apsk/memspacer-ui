{-# LANGUAGE TypeFamilies, GADTs, FlexibleContexts, LambdaCase, RecordWildCards, RankNTypes,
             ScopedTypeVariables, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module MemspacerUI where

import BindUI

import Data.Char
import Data.List (intercalate, elemIndex)
import Data.Time
import Data.Default
import Data.Maybe
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

(<:=) = writeIORef

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

(~$) widget signal action = on widget signal (action widget)

data AddProfileDialog = AddProfileDialog
  { addProfileW  :: Window
  , okB, cancelB :: Button
  , profileNameE :: Entry }

data MainWindow = MainWindow
  { mainW                             :: Window
  , defaultModeRB, shiftingModeRB     :: RadioButton
  , buffLenE, blinkTimeE, intervalE   :: Entry
  , zRotCB, colorCB, soundCB, spaceCB :: CheckButton
  , addTB, saveTB, revertTB           :: ToolButton
  , runB, exitB                       :: Button
  , profileCBX                        :: ComboBox }

addProfileDialog = do
  AddProfileDialog{..} <- $(bind ''AddProfileDialog "add-profile.glade")
  let close = widgetHideAll addProfileW >> mainQuit
  result <- newIORef Nothing
  okB ~$ buttonActivated $ \_ -> do
    profileName <- entryGetText profileNameE
    result <:= Just profileName
    close
  cancelB ~$ buttonActivated $ \_ -> result <:= Nothing >> close
  addProfileW ~$ deleteEvent $ \_ -> liftIO close >> return True
  return $ do
    widgetShowAll addProfileW
    mainGUI
    readIORef result

memspacerUI initialConfigKey initialConfig initialProfilesList = do
  MainWindow{..} <- $(bind ''MainWindow "main.glade")
  configKey <- newIORef initialConfigKey
  config <- newIORef initialConfig
  profilesRef <- newIORef initialProfilesList
  askProfileName <- addProfileDialog
  let getConfig :: Getting a (ConfigGeneric SqlBackend) a -> IO a
      getConfig lens = (^. lens) <$> readIORef config
  let setConfig = modifyIORef config
  let resetUI = do
        toggleButtonSetActive defaultModeRB =<< (== 0) <$> getConfig mode
        toggleButtonSetActive shiftingModeRB =<< (== 1) <$> getConfig mode
        entrySetText buffLenE =<< show <$> getConfig buffLen
        toggleButtonSetActive zRotCB =<< getConfig useZRot
        toggleButtonSetActive colorCB =<< getConfig useColor
        toggleButtonSetActive soundCB =<< getConfig useSound
        toggleButtonSetActive spaceCB =<< getConfig useSpaceBg
        entrySetText blinkTimeE =<< show <$> getConfig blinkTime
        entrySetText intervalE =<< show <$> getConfig interval
        cbItems <- comboBoxSetModelText profileCBX
        profiles <- readIORef profilesRef
        profile <- getConfig profile
        mapM_ (listStoreAppend cbItems) profiles
        comboBoxSetActive profileCBX $ fromJust $ elemIndex profile profiles
  let switchProfile profileName = do
        (cfgKey, cfg) <- db $ configFor profileName
        configKey <:= cfgKey
        config <:= cfg
        resetUI
  resetUI
  mainW `onDestroy` mainQuit
  defaultModeRB ~$ toggled $
    \b -> whenM (toggleButtonGetActive b) $ setConfig (mode .~ 0)
  shiftingModeRB ~$ toggled $
    \b -> whenM (toggleButtonGetActive b) $ setConfig (mode .~ 1)
  buffLenE ~$ editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (buffLen .~ maybe 1 id (readMaybe val))
  zRotCB ~$ toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useZRot .~ tog)
  colorCB ~$ toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useColor .~ tog)
  soundCB ~$ toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useSound .~ tog)
  spaceCB ~$ toggled $
    \b -> toggleButtonGetActive b >>= \tog -> setConfig (useSpaceBg .~ tog)
  blinkTimeE ~$ editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (blinkTime .~ maybe 1 id (readMaybe val))
  intervalE ~$ editableChanged $
    \e -> entryGetText e >>= \val -> setConfig (interval .~ maybe 1 id (readMaybe val))
  addTB ~$ toolButtonClicked $
    \_ -> askProfileName >>= maybe (return ()) switchProfile
  saveTB ~$ toolButtonClicked $
    \_ -> db =<< replace <$> readIORef configKey <*> readIORef config
  revertTB ~$ toolButtonClicked $
    \_ -> ((config <:=) =<< snd <$> db (configFor =<< activeProfile)) >> resetUI
  runB ~$ buttonActivated $
    \_ -> putStrLn =<< cmdArgs <$> readIORef config
  exitB ~$ buttonActivated $
    \_ -> exitSuccess
  widgetShowAll mainW

main = do
  (configKey, config, profiles) <- db $ do
    runMigration migrateAll
    (cfgKey, cfg) <- configFor =<< activeProfile
    profiles <- map (configProfile . entityVal) <$> selectList [] []
    return (cfgKey, cfg, profiles)
  initGUI
  memspacerUI configKey config profiles
  mainGUI
