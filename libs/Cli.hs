{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{- | Cli

A simple experimental terminal user interface (tui) library inspired by elm-ui.
-}
module Cli where

import Data.List (intersperse)
import Data.String (IsString (fromString))
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T
import Utils ((...))


newtype Style = Style Text


data Element = Element [Style] Text


instance IsString Element where
  fromString :: String -> Element
  fromString s = Element [] (T.pack s)


instance Show Element where
  show :: Element -> String
  show = unpack . styled


-- Lib

esc :: Text
esc = "\ESC"


csi :: Text
csi = esc <> "["


resetStyle :: Text
resetStyle = csi <> "0m"


-- Control

{- | Clears the current line where the cursor is at!

    ? Or we could make a special Type that handles cursor controls or erase functions. Maybe a "layout" type
-}
clearLine :: Element
clearLine = unstyled (csi <> "2K")


moveUp :: Int -> Element
moveUp n = unstyled $ csi <> fromString (show n) <> "A"


moveRight :: Int -> Element
moveRight n = unstyled (csi <> (fromString . show) n <> "C")


-- IO

putStyledLn :: [Style] -> [Element] -> IO ()
putStyledLn attrs = T.putStrLn . styled . line attrs


putLines :: [Element] -> IO ()
putLines = T.putStrLn . layout [] . column []


--

layout :: [Style] -> Element -> Text
layout = applyStylesWith


columnLayout :: [Style] -> [Element] -> Text
columnLayout = layout [] ... column


column :: [Style] -> [Element] -> Element
column attrs els =
  Element attrs (join' $ intersperse "\n" $ applyStylesWith attrs <$> els)


line :: [Style] -> [Element] -> Element
line attrs els = Element attrs (join' $ applyStylesWith attrs <$> els)


join' :: [Text] -> Text
join' = foldr (<>) ""


el :: [Style] -> Text -> Element
el = Element


unstyled :: Text -> Element
unstyled = Element []


styled :: Element -> Text
styled (Element [] text) = text
styled (Element styles text) = addStyle styles <> text <> resetStyle


applyStylesWith :: [Style] -> Element -> Text
applyStylesWith [] (Element [] text) = text
applyStylesWith parentStyles (Element styles text) = addStyle (parentStyles <> styles) <> text <> resetStyle


addStyle :: [Style] -> Text
addStyle [] = ""
addStyle styles = csi <> join' (intersperse ";" (unwrapStyles styles)) <> "m"


unwrapStyles :: [Style] -> [Text]
unwrapStyles = (unwrapStyle <$>)


unwrapStyle :: Style -> Text
unwrapStyle (Style s) = s


{- Styles

    Code Sequence   Reset Sequence  Description
    ESC[1m          ESC[21m         bold mode.
    ESC[2m          ESC[22m         dim/faint mode.
    ESC[3m          ESC[23m         italic mode.
    ESC[4m          ESC[24m         underline mode.
    ESC[5m          ESC[25m         blinking mode
    ESC[7m          ESC[27m         inverse/reverse mode
    ESC[8m          ESC[28m         hidden/invisible mode
    ESC[9m          ESC[29m         strikethrough mode.
    ESC[0m   reset all modes (styles and colors)
    ESC[1;34;{...}m   Set graphics modes for cell, separated by semicolon (;).
-}

bold :: Style
bold = Style "1"


dim :: Style
dim = Style "2"


italic :: Style
italic = Style "3"


underline :: Style
underline = Style "4"


blinking :: Style
blinking = Style "5"


inverse :: Style
inverse = Style "7"


hidden :: Style
hidden = Style "8"


strikethrough :: Style
strikethrough = Style "9"


{- | Standard Color
  Color    Foreground   Background Color Code
  Black    30           40
  Red      31           41
  Green    32           42
  Yellow   33           43
  Blue     34           44
  Magenta  35           45
  Cyan     36           46
  White    37           47
  Default  39           49
-}
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default


data ColorPosition
  = Foreground
  | Background


data ColorStyle
  = ColorStyle ColorPosition Color


color :: ColorPosition -> Color -> Style
color p = getColor . ColorStyle p


bgColor :: Color -> Style
bgColor = color Background


fgColor :: Color -> Style
fgColor = color Foreground


getPositionColorCode :: ColorPosition -> Text
getPositionColorCode =
  \case
    Foreground ->
      "3"
    Background ->
      "4"


getColorCodeRaw :: Color -> Text
getColorCodeRaw =
  \case
    Black ->
      "0"
    Red ->
      "1"
    Green ->
      "2"
    Yellow ->
      "3"
    Blue ->
      "4"
    Magenta ->
      "5"
    Cyan ->
      "6"
    White ->
      "7"
    Default ->
      "9"


getColor :: ColorStyle -> Style
getColor (ColorStyle p c) =
  Style $ getPositionColorCode p <> getColorCodeRaw c
