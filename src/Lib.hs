{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( handleArgs
    ) where


import Text.HTML.Scalpel ( URL, Scraper, scrapeStringLike, (@:), (@=), (//)
                         , texts, text, attr, attrs, chroots, chroot, hasClass)
import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy (ByteString, isSuffixOf)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (sort, sortBy, group, isPrefixOf, find, (\\))
import Control.Monad (join)
import Data.Maybe (fromJust)


type Tag = String


type Map = String


data Game = Game {
    date :: String
  , team1 :: Tag
  , team2 :: Tag
  , score :: String
  , link :: URL
} deriving Show


data Veto = Veto_Veto3 Veto3 | Veto_Veto1 Veto1 deriving Show


data Veto3 = Veto3 {
      teamBans :: (String, String)
    , teamPick :: String
    , oppBans :: (String, String)
    , oppPick :: String
    , leftover :: String
  } deriving (Show)


data Veto1 = Veto1 {
      tBans :: (String, String, String)
    , oBans :: (String, String, String)
    , playMap :: String
  } deriving (Show)

maps :: [String]
maps = ["de_cache", "de_mirage", "de_inferno", "de_cbble", "de_overpass"
       , "de_nuke", "de_train"]


handleArgs :: [String] -> IO ()
handleArgs [url] = do
  teamPage <- get url
  let body = teamPage ^. responseBody
      links' = scrapeStringLike body allSeasons
  case links' of
    Nothing -> putStrLn "Could not scrape seasons."
    Just links -> do
      let name''' = scrapeStringLike body teamName
      case name''' of
        Nothing    -> putStrLn "Could not scrape team name."
        Just name'' -> do
          seasonPages <- traverse get links
          let (name', tag') = break (== '(') name''
              name = init name'
              tag = (init . tail) tag'
              seasonBodies = fmap (^. responseBody) seasonPages 
              games' = traverse (flip scrapeStringLike allGames) seasonBodies 
          case games' of
            Nothing -> putStrLn "Could not scrape games."
            Just games -> do
              let teamGames = filter (matchesTeam tag) (join games)
              matches <- mapM get $ link <$> teamGames
              let matchBodies = (^. responseBody) <$> matches
                  logs' = traverse (flip scrapeStringLike logEntries) matchBodies
              case logs' of
                Nothing -> putStrLn "Could not scrape logs."
                Just logs -> do
                  let pairs = zip teamGames logs
                      withVeto = fmap (\ps@(g, _) -> (g, parseMaps . fromJust $ findVeto ps)) (filter hasVeto pairs)
                      vetos = foldr accumulateVetos ([],[],[],[]) $ parseVeto tag <$> (drop ((length withVeto) - 7) withVeto)
                      result = countVetos vetos
                  putStrLn $ "Vetos for: " ++ name ++ " (" ++ tag ++ ")"
                  putStrLn ""
                  prettyPrint result

handleArgs _     = putStrLn "usage: 99veto URL"


teamName :: Scraper ByteString String
teamName = unpack <$> chroot
            ("div" @: ["style" @= "min-height: 120px; margin-left: 115px;"])
            (text "h2")


allSeasons :: Scraper ByteString [URL]
allSeasons = chroots ("table" @: ["style" @= "width: 100%;"] // "tr") season


season :: Scraper ByteString URL
season = do
  links <- attrs "href" "a"
  return . unpack .  last $ links


allGames :: Scraper ByteString [Game]
allGames = chroots ("table" @: [hasClass "league_table_matches"] // "tr") game


game :: Scraper ByteString Game
game = do
  info <- texts ("td" // "a")
  link <- attr "href" ("td" // "a")
  let [date, t1', t2', score] = unpack <$> info
      t1 = drop 1 t1'
      t2 = drop 5 t2'
  return $ Game date t1 t2 score (unpack link)


matchesTeam :: Tag -> Game -> Bool
matchesTeam tag game = team1 game == tag || team2 game == tag


logEntries :: Scraper ByteString [String]
logEntries = chroots (("table" @: ["id" @= "match_log"]) // "tr" // "td") $ do
  content <- text "td"
  return $ unpack content


hasVeto :: (Game, [String]) -> Bool
hasVeto pairs = case findVeto pairs of
                        Nothing -> False
                        Just _  -> True

findVeto :: (Game, [String]) -> Maybe String
findVeto (game, logs) = find (\msg -> "T1 bans" `isPrefixOf` msg) logs


parseMaps :: String -> [String]
parseMaps veto = every 3 . words $ filter (/= ',') veto


parseVeto :: Tag -> (Game, [String]) -> Veto
parseVeto tag (game, veto)
  | tag == team1 game = if score game == ("1:0" :: String) || score game == ("0:1" :: String)
    then Veto_Veto1 $ Veto1 (b1, b3, p1) (b2, b4, p2) p0
    else if length veto == 7
      then Veto_Veto3 $ Veto3 (b1, b4) p2 (b2, b3) p1 p0
      else Veto_Veto3 $ Veto3 (b1, b4) p2 (b2, b3) p1 left
  | otherwise = if score game == ("1:0" :: String) || score game == ("0:1" :: String)
    then Veto_Veto1 $ Veto1 (b2, b4, p2) (b1, b3, p1) p0
    else if length veto == 7
      then Veto_Veto3 $ Veto3 (b2, b3) p1 (b1, b4) p2 p0
      else Veto_Veto3 $ Veto3 (b2, b3) p1 (b1, b4) p2 left
  where
      b1 = veto !! 0
      b2 = veto !! 1
      b3 = veto !! 2
      b4 = veto !! 3
      p1 = veto !! 4
      p2 = veto !! 5
      p0 = veto !! 6
      [left] = maps \\ veto


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


accumulateVetos :: Veto 
                -> ([Map], [Map], [Map], [Map]) 
                -> ([Map], [Map], [Map], [Map]) 
accumulateVetos (Veto_Veto3 (Veto3 (b1, b2) p1 _ p2 l)) (bans, picks, oppPicks, leftovers) = 
  (b1 : b2 : bans, p1 : picks, p2 : oppPicks, l : leftovers)
accumulateVetos (Veto_Veto1 (Veto1 (b1, b2, b3) _ l)) (bans, picks, oppPicks, leftovers) =
  (b1 : b2 : b3 : bans, picks, oppPicks, l : leftovers)


countVetos :: ([Map], [Map], [Map], [Map]) 
           -> ([(Map, Int)], [(Map, Int)], [(Map, Int)], [(Map, Int)])
countVetos (bans, picks, oppPicks, leftovers) = 
  ( sortBy (flip compareKV) $ zip banMaps banCounts
  , sortBy (flip compareKV) $ zip pickMaps pickCounts
  , sortBy (flip compareKV) $ zip oppPickMaps oppPickCounts
  , sortBy (flip compareKV) $ zip leftoverMaps leftoverCounts)
    where 
      compareKV (_, x) (_, y)
        | x > y = GT
        | x < y = LT
        | otherwise = EQ
      bans' = group $ sort bans
      banCounts = length <$> bans'
      banMaps = head <$> bans'
      picks' = group $ sort picks
      pickCounts = length <$> picks'
      pickMaps = head <$> picks'
      oppPicks' = group $ sort oppPicks
      oppPickCounts = length <$> oppPicks'
      oppPickMaps = head <$> oppPicks'
      leftovers' = group $ sort leftovers
      leftoverCounts = length <$> leftovers'
      leftoverMaps = head <$> leftovers'


prettyPrint :: ([(Map, Int)], [(Map, Int)], [(Map, Int)], [(Map, Int)]) -> IO ()
prettyPrint (bans, picks, oppPicks, leftovers) = do
  putStrLn "BANS"
  putStrLn "----------"
  traverse prettyMap bans
  putStrLn ""
  putStrLn "PICKS"
  putStrLn "----------"
  traverse prettyMap picks
  putStrLn ""
  putStrLn "OPP-PICKS"
  putStrLn "----------"
  traverse prettyMap oppPicks
  putStrLn ""
  putStrLn "LEFTOVERS"
  putStrLn "----------"
  traverse prettyMap leftovers
  putStrLn ""

    where
      prettyMap (map, count) 
        | map /= "de_nuke" = putStrLn $ map ++ "\t\t" ++ show count
        | otherwise        = putStrLn $ map ++ "\t\t\t" ++ show count
