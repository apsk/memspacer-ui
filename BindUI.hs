{-# LANGUAGE TemplateHaskell #-}

module BindUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Text.Printf
import Language.Haskell.TH

unQ = mkName . nameBase

nameLit = stringE . nameBase

recFields (TyConI (DataD _ _ _ (ctor:_) _)) = conFields ctor
recFields _                                 = []

conFields (RecC _ fields) = map (\(nm,_,ty) -> (unQ nm,ty)) fields
conFields _               = []

castFor (ConT nm) = varE $ case nameBase nm of
  "Window"      -> 'castToWindow
  "Button"      -> 'castToButton
  "RadioButton" -> 'castToRadioButton
  "CheckButton" -> 'castToCheckButton
  "ToolButton"  -> 'castToToolButton
  "Entry"       -> 'castToEntry
  "ComboBox"    -> 'castToComboBox
  "TreeView"    -> 'castToTreeView
  _             -> error $ printf "typeNameToCastName: unsupported typename `%s'!" (show nm)

bind tyNm file = do
  fields <- fmap recFields (reify tyNm)
  let bindF ui (nm,ty) = bindS (varP nm)
        [| xmlGetWidget $ui $(castFor ty) $(nameLit nm) |]
      binds ui = map (bindF ui) fields
      ctorApp = [| return $(foldl appE (conE $ unQ tyNm) (map (varE . fst) fields)) |]
  [| do Just ui <- xmlNew $(stringE file)
        $(doE (binds [|ui|] ++ [noBindS ctorApp])) |]

-- # To test `bind'-macro:
-- data MainWindow = MainWindow { mainW :: Window, okB :: Button }
-- stx = $(stringE =<< runQ $ bind ''MainWindow "main-window.glade")

{- # Approach before macros, more on
       http://stackoverflow.com/questions/20803484/constraints-in-explicit-signatures-for-monadic-bindings

newtype WidgetBinder = WidgetBinder
  (WidgetClass w => (GObject -> w) -> String -> IO w)

widgetBinder file = do
  Just ui <- xmlNew file
  return $ WidgetBinder (xmlGetWidget ui)
-}
