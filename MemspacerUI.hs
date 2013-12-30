{-# LANGUAGE TypeFamilies, GADTs, FlexibleContexts, RecordWildCards, RankNTypes,
             QuasiQuotes, TemplateHaskell, OverloadedStrings, LambdaCase #-}

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
(<:<) ref mval = writeIORef ref =<< mval

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
      rightAnswers  Int
      wrongAnswers  Int
      missedAnswers Int
      deriving Show
  |]

makeLensesFor (map (\nm@(c:cs) -> ("config" ++ toUpper c : cs, nm))
                 [ "profile", "mode", "buffLen", "useZRot"
                 , "useColor", "useSound", "useSpaceBg"
                 , "idleColor" , "blinkColor", "blinkTime", "interval" ])
  ''ConfigGeneric

makeLensesFor (map (\nm@(c:cs) -> ("session" ++ toUpper c : cs, nm))
                 [ "pid", "date", "rightAnswers"
                 , "wrongAnswers", "missedAnswers" ])
  ''SessionGeneric

instance Default (ConfigGeneric backend) where
  def = Config "default" 0 1 False False False False 0 0 1 3

cmdArgs :: ConfigGeneric backend -> String
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

activeConfig = configFor =<< activeProfile

db = runSqlite "mspc.db"

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= \b -> when b action

toolButtonClicked :: Signal ToolButton (IO ())
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
  , profileCBX                        :: ComboBox
  , historyTV                         :: TreeView }

newtype ColumnInserter s = ColumnInserter
  (Show a => String -> Getting a s a -> IO Int)

columnInserter treeView model = ColumnInserter $ \title lens -> do
  column <- treeViewColumnNew
  treeViewColumnSetTitle column title
  renderer <- cellRendererTextNew
  cellLayoutPackStart column renderer False
  cellLayoutSetAttributes column renderer model $ \entry ->
    [cellText := show (entry ^. lens)]
  treeViewAppendColumn treeView column

initHistory historyTV = do
  sessions <- listStoreNew []
  treeViewSetModel historyTV sessions
  treeViewSetHeadersVisible historyTV True
  let ColumnInserter addCol = columnInserter historyTV sessions
  addCol "Дата" date
  addCol "Вірні відповіді" rightAnswers
  addCol "Невірні відповіді" wrongAnswers
  addCol "Пропущені відповіді" missedAnswers
  return sessions

addProfileDialog :: IO (IO (Maybe String))
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

memspacerUI initialConfig initialProfilesList = do
  MainWindow{..} <- $(bind ''MainWindow "main.glade")
  config   <- newIORef initialConfig
  profiles <- newIORef initialProfilesList
  sessions <- initHistory historyTV
  askProfileName <- addProfileDialog
  let
    getConfig :: Getting a (ConfigGeneric SqlBackend) a -> IO a
    getConfig lens = (^. lens) . snd <$> readIORef config
    setConfig = modifyIORef config . fmap
    initUI = do
      cbItems <- comboBoxSetModelText profileCBX
      mapM_ (listStoreAppend cbItems) initialProfilesList
      curProfile <- getConfig profile
      comboBoxSetActive profileCBX $ fromJust $
        elemIndex curProfile initialProfilesList
    resetUI = do
      toggleButtonSetActive defaultModeRB =<< (== 0) <$> getConfig mode
      toggleButtonSetActive shiftingModeRB =<< (== 1) <$> getConfig mode
      entrySetText buffLenE =<< show <$> getConfig buffLen
      toggleButtonSetActive zRotCB =<< getConfig useZRot
      toggleButtonSetActive colorCB =<< getConfig useColor
      toggleButtonSetActive soundCB =<< getConfig useSound
      toggleButtonSetActive spaceCB =<< getConfig useSpaceBg
      entrySetText blinkTimeE =<< show <$> getConfig blinkTime
      entrySetText intervalE =<< show <$> getConfig interval
    switchProfile profileName = do
      config <:< db (configFor profileName)
      resetUI
  initUI
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
  profileCBX ~$ changed $
    \_ -> switchProfile . fromJust =<< comboBoxGetActiveText profileCBX
  addTB ~$ toolButtonClicked $
    \_ -> askProfileName >>= maybe (return ()) switchProfile
  saveTB ~$ toolButtonClicked $
    \_ -> db =<< uncurry replace <$> readIORef config
  revertTB ~$ toolButtonClicked $
    \_ -> (config <:< db activeConfig) >> resetUI
  runB ~$ buttonActivated $
    \_ -> putStrLn =<< cmdArgs . snd <$> readIORef config
  exitB ~$ buttonActivated $
    \_ -> exitSuccess
  widgetShowAll mainW

main = do
  (config, profiles) <- db $ do
    runMigration migrateAll
    (,) <$> activeConfig
        <*> (map (configProfile . entityVal) <$> selectList [] [])
  initGUI
  memspacerUI config profiles
  mainGUI
