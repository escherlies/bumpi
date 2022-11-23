{-# LANGUAGE InstanceSigs #-}

module Cli where

import Data.List (intersperse)
import Data.String (IsString (fromString))
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T


esc :: Text
esc = "\ESC"


resetStyle :: Text
resetStyle = esc <> "[0m"


data Element = Element [Style] Text


instance IsString Element where
  fromString :: String -> Element
  fromString s = Element [] (T.pack s)


instance Show Element where
  show :: Element -> String
  show = unpack . styled


putSample :: IO ()
putSample = T.putStrLn sample


sample :: Text
sample =
  styled $
    line
      [ underline
      , bgColor Cyan
      ]
      [ el
          [ bgColor Red
          , fgColor Yellow
          ]
          "Hello, "
      , "awesome "
      , el
          [ bgColor Magenta
          , fgColor Black
          ]
          "World!"
      , "\n"
      , line
          []
          ( intersperse
              "\n"
              [ el [bold] "bold"
              , el [dim] "dim"
              , el [italic] "italic"
              , el [underline] "underline"
              , el [blinking] "blinking"
              , el [inverse] "inverse"
              , el [hidden] "hidden"
              , el [strikethrough] "strikethrough"
              ]
          )
      ]


{- | Clears the current line where the cursor is at!

    ? Or we could make a special Type that handles cursor controls or erase functions. Maybe a "layout" type
-}
clearLine :: Element
clearLine = unstyled (esc <> "[2K")


-- | TODO Add more curor controls
moveUp :: Int -> Element
moveUp n = unstyled $ esc <> "[" <> fromString (show n) <> "A"


putStyledLn :: [Style] -> [Element] -> IO ()
putStyledLn attrs = T.putStrLn . styled . line attrs


putLines :: [Element] -> IO ()
putLines = T.putStrLn . lns


lns :: [Element] -> Text
lns = foldl (<>) "" . intersperse "\n" . (styled <$>)


line :: [Style] -> [Element] -> Element
line attrs els =
  el
    attrs
    (join' $ applyStylesWith attrs <$> els)


join' :: [Text] -> Text
join' = foldr (<>) ""


el :: [Style] -> Text -> Element
el = Element


unstyled :: Text -> Element
unstyled = Element []


styled :: Element -> Text
styled (Element styles text) =
  addStyle styles <> text <> resetStyle


applyStylesWith :: [Style] -> Element -> Text
applyStylesWith parentStyles (Element styles text) =
  addStyle (parentStyles <> styles) <> text <> resetStyle


addStyle :: [Style] -> Text
addStyle styles =
  (esc <> "[")
    <> join' (intersperse ";" (unwrapStyles styles))
    <> "m"


unwrapStyles :: [Style] -> [Text]
unwrapStyles = (unwrapStyle <$>)


unwrapStyle :: Style -> Text
unwrapStyle (Style s) = s


{- |

ESC Code Sequence 	Reset Sequence 	Description
ESC[1;34;{...}m 		Set graphics modes for cell, separated by semicolon (;).
ESC[0m 		reset all modes (styles and colors)
ESC[1m 	ESC[21m 	set bold mode.
ESC[2m 	ESC[22m 	set dim/faint mode.
ESC[3m 	ESC[23m 	set italic mode.
ESC[4m 	ESC[24m 	set underline mode.
ESC[5m 	ESC[25m 	set blinking mode
ESC[7m 	ESC[27m 	set inverse/reverse mode
ESC[8m 	ESC[28m 	set hidden/invisible mode
ESC[9m 	ESC[29m 	set strikethrough mode.
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


{- |
  Color |  	Foreground |	Background Color Code
  Black    30    40
  Red      31    41
  Green    32    42
  Yellow   33    43
  Blue     34    44
  Magenta  35    45
  Cyan     36    46
  White    37    47
  Default  39    49
  Reset    0     0
-}
data Colors
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Reset


data ColorPosition
  = Foreground
  | Background


data ColorStyle
  = ColorStyle ColorPosition Colors


bgColor :: Colors -> Style
bgColor = applyStyle . ColorStyle Background


fgColor :: Colors -> Style
fgColor = applyStyle . ColorStyle Foreground


newtype Style = Style Text


applyStyle :: ColorStyle -> Style
applyStyle (ColorStyle p c) =
  getCode p $
    case c of
      Black ->
        ("30", "40")
      Red ->
        ("31", "41")
      Green ->
        ("32", "42")
      Yellow ->
        ("33", "43")
      Blue ->
        ("34", "44")
      Magenta ->
        ("35", "45")
      Cyan ->
        ("36", "46")
      White ->
        ("37", "47")
      Default ->
        ("39", "49")
      Reset ->
        ("0", "0")


getCode :: ColorPosition -> (Text, Text) -> Style
getCode Foreground (fg, _) = Style fg
getCode Background (_, bg) = Style bg
