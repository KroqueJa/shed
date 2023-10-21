{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad (void)
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str)
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A

data Name = Edit
          deriving (Ord, Show, Eq)

data St =
    St { _edit :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e = renderEditor (st^.edit)
        ui = e

renderEditor :: E.Editor String Name -> T.Widget Name
renderEditor e = E.renderEditor (str . unlines) True e

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent ev = do
    zoom edit $ E.handleEditorEvent ev

initialState :: St
initialState =
    St (E.editor Edit Nothing "")


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

shedApp :: M.App St e Name
shedApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = const $ M.showCursorNamed Edit
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    void $ M.defaultMain shedApp initialState
