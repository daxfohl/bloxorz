module Maps (
  Level(..),
  getMap,
  maps
) where


data Level = Level { lMap      :: [String]
                   , lStartPos :: (Int,Int)
                   } deriving Show


maps = [{-Level { lMap = ["bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbebbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb",
                        "bbbbbbbbbbbbbbb"]
                , lStartPos = (0,0)
              },-}
        Level { lMap = ["               ",
                        "               ",
                        "  bbb          ",
                        "  bbbbbb       ",
                        "  bbbbbbbbb    ",
                        "   bbbbbbbbb   ",
                        "       bbebb   ",
                        "        bbb    ",
                        "               ",
                        "               "]
              , lStartPos = (3,3)
              },
        Level { lMap = ["               ",
                        "               ",
                        "      bbbbbbb  ",
                        "bbbb  bbb  bb  ",
                        "bbbbbbbbb  bbbb",
                        "bbbb       bbeb",
                        "bbbb       bbbb",
                        "            bbb",
                        "               ",
                        "               "]
              , lStartPos = (1,5)
              },
        Level { lMap = ["     bbbbbb    ",
                        "     b  bbb    ",
                        "     b  bbbbb  ",
                        "bbbbbb     bbbb",
                        "    bbb    bbeb",
                        "    bbb     bbb",
                        "      b  bb    ",
                        "      bbbbb    ",
                        "      bbbbb    ",
                        "       bbb     "]
              , lStartPos = (0,3)
              }

       ]


getMap level x y =
  if 0 <= x && x <= 14 &&
     0 <= y && y <= 9
    then (lMap level !! y) !! x
    else ' '
