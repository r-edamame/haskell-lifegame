{-# LANGUAGE TemplateHaskell #-}

module LifeGame.Types where

import Control.Lens

import Data.Array (Array)
import Linear.V2 (V2(..))

type Field = Array (V2 Int) (Maybe Cell)

data Cell = RedCell | BlueCell deriving (Eq,Ord)

data Direction = U | D | R | L

data Game = Game {
    _cells :: Field
  , _cellSize :: V2 Int
  , _fieldSize :: V2 Int
  , _setObject :: [V2 Int]
  , _setDirection :: Direction
  , _screenSize :: (Int,Int)
  , _surviveRule :: [Int]
  , _bornRule :: [Int]
  }
makeLenses ''Game
