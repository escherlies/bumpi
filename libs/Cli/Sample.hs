module Sample where


putSample :: IO ()
putSample = T.putStrLn $ styled sample


sample :: Element
sample =
  column
    []
    [ line
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
        ]
    , line
        []
        ( intersperse
            " "
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
    , column
        [ underline
        , bgColor Cyan
        ]
        [ "Liene 0  "
        , line [dim] ["Line 1"]
        , line
            []
            [ "Line 2.1"
            , " Line 2.2"
            , el [bold] " Line 2.3"
            , line [] [" Line 2.4"]
            -- \^^^^^^^^^^^^^^^^^^^ FIXME This has no style
            ]
        , line [italic] ["Line 3"]
        , line [] ["Line 4"]
        , "Line 5"
        ]
    ]
