{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( handleArgs
    , getTeamInfo
    ) where


import Text.HTML.Scalpel.Core ( Scraper, scrapeStringLike, (@:), (@=), (//)
                              , texts, text, attr, attrs, chroots, chroot
                              , hasClass)
import Network.Wreq (responseBody)
import Network.Wreq.Session (Session, newSession, get)
import Control.Lens ((^.))
import Data.ByteString.Lazy (ByteString, isSuffixOf)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (sort, sortBy, group, isPrefixOf, find, (\\))
import Control.Monad (join)
import Control.Applicative (liftA3)
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)


-- | Alias for a web address
type URL = String


-- | Alias for the short version of a team's name.
type Tag = String


-- | Alias for the name of the map / level.
type Map = String


-- | Holds information about the team that is to be analyzed
data Team = Team {
    name :: String -- ^ The full name of the team
  , tag  :: Tag -- ^ The tag of the team
  , seasons :: [URL] -- ^ Links to the season-pages of the seasons that the team participated in
}


-- | Holds information about a single match / game.
data Game = Game {
    date  :: String -- ^ Date the match was played on
  , team1 :: Tag -- ^ Team A / left team
  , team2 :: Tag -- ^ Team B / right team
  , score :: String -- ^ Outcome of the match
  , link  :: URL -- ^ URL to the webpage of this match
} deriving Show


-- | Sum type that holds information about either a bo3 or a bo1 match.
data Veto = Veto_Veto3 Veto3 | Veto_Veto1 Veto1 deriving Show


-- | Holds information about the veto-process of a best-of-3 match.
data Veto3 = Veto3 {
      teamBans :: (String, String) -- ^ Maps that were banned by the team that is being analyzed
    , teamPick :: String -- ^ Map that was picked by the team that is being analyzed
    , oppBans  :: (String, String) -- ^ Maps that were banned by the opposing team
    , oppPick  :: String -- ^ Map that was picked by the opposing team
    , leftover :: String -- ^ Map that was neither banned nor picked
  } deriving Show


-- | Holds information about the veto-process of a best-of-1 match.
data Veto1 = Veto1 {
      tBans   :: (String, String, String) -- ^ Maps that were banned by the team that is being analyzed
    , oBans   :: (String, String, String) -- ^ Maps that were banned by the opposing team
    , playMap :: String -- ^ Map that was played in this bo1-match
  } deriving Show


-- | All maps that are currently in the active-duty group.
maps :: [Map]
maps = ["de_cache", "de_mirage", "de_inferno", "de_cbble", "de_overpass"
       , "de_nuke", "de_train"]


-- | States how many matches the program should analyze.
numberMatchesToAnalyze :: Int
numberMatchesToAnalyze = 7


-- | Main function of the program.
handleArgs :: [String] -> IO ()
handleArgs ["--help"] = putStrLn "Usage: 99veto URL"

handleArgs [url] = do
  sess <- newSession
  -- get team name, tag and seasons
  mbTeamInfo <- getTeamInfo sess url
  case mbTeamInfo of
    Nothing -> putStrLn "Could not scrape team info."
    Just teamInfo -> do

          -- get team matches
          mbTeamGames <- getTeamGames sess teamInfo
          case mbTeamGames of
            Nothing -> putStrLn "Could not scrape games."
            Just teamGames -> do

              -- get vetos - tuple (game, veto)?
              -- Now it downloads all matches this team has participated in
              -- or will in the current season.
              matches <- traverse (get sess) $ link <$> teamGames
              let matchBodies = (^. responseBody) <$> matches
                  mbLogs = traverse (flip scrapeStringLike logEntries) matchBodies
              case mbLogs of
                Nothing -> putStrLn "Could not scrape logs."
                Just logs -> do
                  let pairs = zip teamGames logs
                      withVeto = fmap (\ps@(g, _) -> (g, parseMaps . fromJust $ findVeto ps)) (filter hasVeto pairs)

                      -- take the last numberMatchesToAnalyze matches and accumulate
                      vetos = foldr accumulateVetos ([],[],[],[]) $ parseVeto (tag teamInfo) <$> (drop ((length withVeto) - numberMatchesToAnalyze) withVeto)
                      result = countVetos vetos

                  -- output
                  putStrLn $ "Vetos for: " ++ name teamInfo ++ "(" ++ tag teamInfo ++ ")"
                  putStrLn ""
                  prettyPrint result

handleArgs _ = putStrLn "Usage: 99veto URL"


-- | Downloads the teampage specified by the second argument and parses it into
-- a Maybe Team value.
getTeamInfo :: Session -- ^ Session with which wreq will download the page.
            -> URL -- ^ Link to the team-page.
            -> IO (Maybe Team) -- ^ The team-value holding the desired info.
getTeamInfo sess url = do
  teamPage <- get sess url
  let teamBody = teamPage ^. responseBody
      mbLinks = scrapeStringLike teamBody allSeasons
      mbNameAndTag = scrapeStringLike teamBody teamNameAndTag
  return $ liftA3 Team (fst <$> mbNameAndTag) (snd <$> mbNameAndTag) mbLinks


-- | Downloads the pages of the matches the team particepated in and parses them
-- into a list of Games.
getTeamGames :: Session -- ^ Session with which wreq will download the page.
             -> Team -- ^ The team-info of the team of which the matches will be downloaded.
             -> IO (Maybe [Game]) -- ^ Game values holding information about the game.
getTeamGames sess teamInfo = do
  seasonPages <- traverse (get sess) $ seasons teamInfo
  let seasonBodies = fmap (^. responseBody) seasonPages 
      mbGames = join <$> traverse (flip scrapeStringLike allGames) seasonBodies 
      mbTeamGames = filter (matchesTeam $ tag teamInfo) <$> mbGames
  return mbTeamGames


-- | Takes a team's page (body) and produces a tuple containing the name and
-- the tag of the team.
-- > ("Casual Identity", "Casuals")
teamNameAndTag :: Scraper ByteString (String, Tag)
teamNameAndTag = parseNameAndTag . unpack <$> chroot
    ("div" @: ["style" @= "min-height: 120px; margin-left: 115px;"])
    (text "h2")
  where parseNameAndTag = bimap init (init . tail) . break (== '(')  


-- | Takes a team's page (body) and produces a list of all seasons the team
-- participated in.
allSeasons :: Scraper ByteString [URL]
allSeasons = chroots ("table" @: ["style" @= "width: 100%;"] // "tr") season
  where
    season = do
      links <- attrs "href" "a"
      return . unpack .  last $ links


-- | Takes a season's page (body) and produces a list of all games that have
-- been played in this season.
allGames :: Scraper ByteString [Game]
allGames = chroots ("table" @: [hasClass "league_table_matches"] // "tr") game
  where
    game = do
      info <- texts ("td" // "a")
      link <- attr "href" ("td" // "a")
      let [date, t1', t2', score] = unpack <$> info
          t1 = drop 1 t1'
          t2 = drop 5 t2'
      return $ Game date t1 t2 score (unpack link)


-- | Takes a match's paege (body) and produces a list that contains the match's
-- log entries.
logEntries :: Scraper ByteString [String]
logEntries = chroots (("table" @: ["id" @= "match_log"]) // "tr" // "td") $ do
  content <- text "td"
  return $ unpack content


-- | Takes a team's tag and a game and determines if this team participated in
-- this match.
matchesTeam :: Tag -> Game -> Bool
matchesTeam tag game = team1 game == tag || team2 game == tag


-- | Takes a tuple of a game and a list of strings (reprecenting the match-log)
-- and determines if this match has a map-veto. (It might not because e.g. a
-- team did not show up)
hasVeto :: (Game, [String]) -> Bool
hasVeto pairs = case findVeto pairs of
                        Nothing -> False
                        Just _  -> True


-- | Takes a tuple of a game and a list of strings (reprecenting the match-log)
-- and returns Nothing if the match did not have a veto or the Just the string
-- containing the veto.
findVeto :: (Game, [String]) -> Maybe String
findVeto (game, logs) = find (\msg -> "T1 bans" `isPrefixOf` msg) logs


-- | Takes a string containing a map veto and returns a list of only those Maps
parseMaps :: String -> [Map]
parseMaps veto = every 3 . words $ filter (/= ',') veto


-- | Takes a team's tag and a tuple of a game and the corresponding match-log
-- (list of strings) and returns a Veto that contains information about this
-- match's map-veto.
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


-- | Returns every nth element of a list.
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


-- | Prepends a Veto's information to a 4-tuple of
-- (bans, picks, oppPicks, leftovers).
accumulateVetos :: Veto 
                -> ([Map], [Map], [Map], [Map]) 
                -> ([Map], [Map], [Map], [Map]) 
accumulateVetos (Veto_Veto3 (Veto3 (b1, b2) p1 _ p2 l)) (bans, picks, oppPicks, leftovers) = 
  (b1 : b2 : bans, p1 : picks, p2 : oppPicks, l : leftovers)
accumulateVetos (Veto_Veto1 (Veto1 (b1, b2, b3) _ l)) (bans, picks, oppPicks, leftovers) =
  (b1 : b2 : b3 : bans, picks, oppPicks, l : leftovers)


-- | Turns a 4-tuple of (bans, picks, oppPicks, leftovers) into a 4-tuple of
-- associative lists of Maps and the count how often it appeared in one of the
-- original lists (e.g. bans).
countVetos :: ([Map], [Map], [Map], [Map]) 
           -> ([(Map, Int)], [(Map, Int)], [(Map, Int)], [(Map, Int)])
countVetos (bans, picks, oppPicks, leftovers) = 
  ( sortBy (flip compareKV) $ zip banMaps banCounts
  , sortBy (flip compareKV) $ zip pickMaps pickCounts
  , sortBy (flip compareKV) $ zip oppPickMaps oppPickCounts
  , sortBy (flip compareKV) $ zip leftoverMaps leftoverCounts)
    where 
      -- | Compares the values of two key-value-pairs.
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


-- | Pretty prints a 4-tuple of (Map, count)-associative lists to the terminal.
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
