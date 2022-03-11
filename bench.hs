-- #!~/.ghcup/bin/runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# HLINT ignore "Redundant flip" #-}

import Data.Maybe
import Data.Either
import Data.Bifunctor
import System.Process
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char
import Data.Text as T (Text, pack, unpack, replace, splitOn, strip, intercalate, breakOn, tail)
import qualified Data.Text.IO as TIO
import Text.Read
import Data.Map
import Data.List as L
import Data.Monoid
import Control.Category
import Data.Time (parseTime)

main :: IO ()
main = do
  results <- readFile "settings.csv" >>= (pack >>> readSettings >>> traverse runBench)
  TIO.readFile "langs_template.md" >>= (embedResult (rights results) >>> TIO.writeFile "langs.md")


data Settings = Settings {
    settingsid :: Text,
    directory  :: Text,
    langname   :: Text,
    sourcepath :: Text,
    buildcmd   :: Text,
    execcmd    :: Text
  } deriving Show

data BenchResult = BenchResult {
    settings    :: Settings,
    sourcestr   :: Text,
    buildcmd    :: Text,
    buildresult :: Text,
    benchcmd    :: Text,
    benchresult :: Text,
    timeresult  :: (Double, Double, Double)
  } deriving Show

tshow :: Show a => a -> Text
tshow = show >>> pack

readSettings :: Text -> [Settings]
readSettings str = catMaybes $ lineToSettings <$> Prelude.tail (splitOn "\n" str)
  where
    lineToSettings line =
      case strip <$> splitOn "," line of
        sid:dir:nam:src:bld:exe:_ -> Just $ Settings sid dir nam src bld exe
        _                     -> Nothing

runBench :: Settings -> IO (Either Text BenchResult)
runBench settings = do
  handle (\(e :: SomeException) -> (tshow >>> Left >>> pure) e) (do
      source <- pack <$> readCreateProcess ((proc "cat" [unpack $ sourcepath settings]) {cwd = Just $ unpack $ directory settings}) ""

      TIO.putStrLn $ "~~ Start \"" <> settingsid settings <> "\" ~~"
      build <- pack <$> readCreateProcess ((shell $ unpack (buildcmd (settings :: Settings)) ++ " 2>&1") {cwd = Just $ unpack $ directory settings}) ""
      TIO.putStrLn build

      let realcmd = "{ time " <> execcmd settings <> " 2>/dev/null; } 2>&1"
      let cmd = "time " <> execcmd settings
      result <- pack <$> readCreateProcess ((shell $ unpack realcmd) {cwd = Just $ unpack $ directory settings}) ""
      TIO.putStrLn result
      TIO.putStrLn $ "~~ End \"" <> settingsid settings <> "\" ~~\n\n"

      let timeresult = parseTimeResult ((splitOn "\n" >>> reverse >>> L.drop 1 >>> L.take 3 >>> reverse >>> T.intercalate "\n") result)

      pure $ Right $ BenchResult {
          settings    = settings,
          sourcestr   = source,
          buildcmd    = buildcmd (settings :: Settings),
          buildresult = build,
          benchcmd    = cmd,
          benchresult = result,
          timeresult  = timeresult
        }
    )

parseTimeResult :: Text -> (Double, Double, Double)
parseTimeResult str =
  let x_m_y_s_to_seconds = breakOn "m"  >>> second (T.tail >>> breakOn "s" >>> fst) >>> \(mstr, sstr) -> read (unpack mstr) * 60 + read (unpack sstr)
      line_to_seconds    = breakOn "\t" >>> snd >>> x_m_y_s_to_seconds
  in case splitOn "\n" str of
      real:user:sys:_ -> (line_to_seconds real, line_to_seconds user, line_to_seconds sys)
      _               -> (0,0,0)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

embedResult :: [BenchResult] -> Text -> Text
embedResult results =
    flip (L.foldl' (\t result ->
      let langid = (settings >>> settingsid) result
          lang   = (settings >>> directory) result
          code   = "\ncode:\n```" <> lang <> "\n" <> sourcestr result <> "```\n"
          run    = "result:\n```\n" <>
                    "$ " <> buildcmd (result :: BenchResult) <> "\n" <>
                    "$ " <> benchcmd result <> "\n" <> benchresult result <> "```\n"
      in (replace ("{code:"   <> langid <> "}") code >>>
          replace ("{result:" <> langid <> "}") run >>>
          replace ("{sample:" <> langid <> "}") (code <> "\n" <> run)
          ) t
    )) results >>>
    replace "{ranking}" (
          let sortedByReal = (L.map (\res -> ((settings >>> langname) res, (timeresult >>> \(r,u,s) -> r) res)) >>> sortOn snd) results
              sortedByUser = (L.map (\res -> ((settings >>> langname) res, (timeresult >>> \(r,u,s) -> u) res)) >>> sortOn snd) results
              tocol rank (lang, time) = "| " <> tshow (rank+1) <> " | " <> lang <> " | " <> tshow time <> " sec. |\n"
              header = "| rank | lang | time |\n| - | - | - |\n"
          in "実行時間：\n" <> (header <> T.intercalate "" (mapWithIndex tocol sortedByReal)) <> "\n" <>
             "CPU時間：\n" <> (header <> T.intercalate "" (mapWithIndex tocol sortedByUser))
        )
