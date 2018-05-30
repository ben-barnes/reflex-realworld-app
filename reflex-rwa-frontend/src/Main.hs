{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (
  main
) where

import Control.Applicative ((<*>))
import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.Text as T

main :: IO ()
main = mainWidget calculator

app1 :: (MonadWidget t m) => m ()
app1 = el "div" $ text "Welcome to Reflex"

app2 :: (MonadWidget t m) => m ()
app2 =
  el "div" $ do
    el "p" $ text "Reflex is:"
    el "ul" $ do
      el "li" $ text "Efficient"
      el "li" $ text "Higher-order"
      el "li" $ text "Glitch-free"

app3 :: (MonadWidget t m) => m ()
app3 = el "div" $ do
  t <- textInput def
  dynText $ _textInput_value t

app4 :: (MonadWidget t m) => m ()
app4 = el "div" $ do
  t <- textInput def
  text "Last key pressed: "
  let keypressEvent = fmap (T.pack . show) $ _textInput_keypress t
  keypressDyn <- holdDyn "None" keypressEvent
  dynText keypressDyn

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ "0"
                       & textInputConfig_attributes .~ attrs
      let result = fmap (readMaybe . T.unpack) $ _textInput_value n
          attrs = fmap (maybe errorState (const validState)) result
  return result

calculator :: (MonadWidget t m) => m ()
calculator = el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  text " = "
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) (_dropdown_value d) values
  dynText . fmap (T.pack . show) $ result

data Op
  = Plus
  | Minus
  | Times
  | Divide
    deriving (Eq, Ord)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: (Fractional a) => Op -> a -> a -> a
runOp Plus = (+)
runOp Minus = (-)
runOp Times = (*)
runOp Divide = (/)
