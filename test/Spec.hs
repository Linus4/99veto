main :: IO ()
main = putStrLn "Test suite not yet implemented"

exampleURL :: URL
exampleURL = "https://csgo.99damage.de/de/leagues/teams/27363-casual-identity"


testGame :: ByteString
testGame = "<tr> \
      \ <td width=\"90\" class=\"i\"><a href=\"https://csgo.99damage.de/de/leagues/matches/341792\">25 Nov 2017</a></td> \
            \ <td width=\"160\"><a href=\"https://csgo.99damage.de/de/leagues/matches/341792\"><img src=\"https://cdn1.gamesports.net/img/flags/de.gif\" border=\"0\" alt=\"de\" title=\"\" /> NinjaTime</a></td> \
                  \ <td width=\"165\"><a href=\"https://csgo.99damage.de/de/leagues/matches/341792\">vs. <img src=\"https://cdn1.gamesports.net/img/flags/de.gif\" border=\"0\" alt=\"de\" title=\"\" /> MMG</a></td> \
                        \ <td class=\"i\"><a href=\"https://csgo.99damage.de/de/leagues/matches/341792\">2:0</a></td> \
                              \ <td width=\"15\"></td> \
                                  \ </tr>"


exampleVeto :: String
exampleVeto = "T1 bans de_overpass, T2 bans de_train, T2 bans de_nuke, T1 bans de_mirage, T2 picks de_cbble, T1 picks de_cache"

