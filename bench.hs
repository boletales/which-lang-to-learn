#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.Maybe
import Data.Either
import Data.Functor
import Data.Bifunctor
import System.Process
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char
import Data.Text as T (Text, pack, unpack, replace, splitOn, strip, intercalate, breakOn, tail, breakOnEnd, unsnoc, breakOnAll, drop, uncons)
import qualified Data.Text.IO as TIO
import Text.Read
import Text.Printf
import Data.Map
import Data.List as L
import Data.Monoid
import Control.Category

main :: IO ()
main = do
  getArgs >>= (fmap pack >>> \case
    "run":sid:_ -> main_run sid
    args        -> main_bench args)

main_run :: Text -> IO ()
main_run sid = do
  settingsData <- readFile "settings.csv" <&> (pack >>> readSettings)
  case find (settingsid >>> (==sid)) (reverse settingsData) of
    Nothing -> TIO.putStrLn $ "no settings named \"" <> sid <> "\""
    Just settings -> do
      TIO.putStrLn $ "~~ Start \"" <> settingsid (settings :: Settings) <> "\" ~~"
      build <- pack <$> readCreateProcess ((shell $ unpack (buildcmd (settings :: Settings)) ++ " 1>&2") {cwd = Just $ unpack $ directory settings}) ""

      let realcmd = "/bin/bash -c \"" <> "time " <> execcmd settings <> " 1>&2" <> "\""
      let cmd = "time " <> execcmd settings
      result <- pack <$> readCreateProcess ((shell $ unpack realcmd) {cwd = Just $ unpack $ directory settings}) ""
      TIO.putStrLn result
      TIO.putStrLn $ "~~ End \"" <> settingsid settings <> "\" ~~"
  
main_bench :: [Text] -> IO ()
main_bench args = do
  let fullbench = elem "--full" args || elem "-f" args
  let nomd = elem "--no-md" args || elem "-n" args
  let nobuild = elem "--no-build" args || elem "-n" args
  files   <- readFile "files.csv"    >>= (pack >>> readFileSettings >>> traverse getSource >>> fmap rights)
  results <- readFile "settings.csv" >>= (pack >>> readSettings >>> L.filter (\s -> not (optional s) || fullbench) >>> traverse (runBench nobuild)  >>> fmap rights)
  unless nomd $ TIO.readFile "langs_template.md" >>= (embed files results >>> TIO.writeFile "langs.md")
  TIO.putStrLn "~~~~~~~~"
  TIO.putStrLn $ rankingStr results

data Settings = Settings {
    settingsid :: Text,
    directory  :: Text,
    langname   :: Text,
    sourcepath :: Text,
    buildcmd   :: Text,
    execcmd    :: Text,
    optional   :: Bool
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

data SourceFile = SourceFile {
    fileid   :: Text,
    filelang :: Text,
    filepath :: Text
  } deriving Show

tshow :: Show a => a -> Text
tshow = show >>> pack

readSettings :: Text -> [Settings]
readSettings str = catMaybes $ lineToSettings <$> Prelude.tail (splitOn "\n" str)
  where
    lineToSettings line =
      case strip <$> splitOn "," line of
        sid:dir:nam:src:bld:exe:_ ->
            case T.uncons sid of
                Just ('!', sid') -> Just $ Settings sid' dir nam src bld exe True
                _                -> Just $ Settings sid  dir nam src bld exe False
        _                         -> Nothing

readFileSettings :: Text -> [SourceFile]
readFileSettings str = catMaybes $ lineToSettings <$> Prelude.tail (splitOn "\n" str)
  where
    lineToSettings line =
      case strip <$> splitOn "," line of
        sid:lang:path:_ -> Just $ SourceFile sid lang path
        _                     -> Nothing

getSource :: SourceFile -> IO (Either Text (SourceFile, Text))
getSource sf =
  handle (\(e :: SomeException) -> (tshow >>> Left >>> pure) e) (do
      (pack >>> (sf,) >>> Right) <$> readCreateProcess (proc "cat" [unpack $ filepath sf]) ""
    )

runBench :: Bool -> Settings -> IO (Either Text BenchResult)
runBench nobuild settings = do
  handle (\(e :: SomeException) -> (tshow >>> Left >>> pure) e) (do
      source <- pack <$> readCreateProcess ((proc "cat" [unpack $ sourcepath settings]) {cwd = Just $ unpack $ directory settings}) ""

      TIO.putStrLn $ "~~ Start \"" <> settingsid settings <> "\" ~~"
      build <- 
        if nobuild
          then pure ""
          else (do
            build <- pack <$> readCreateProcess ((shell $ unpack (buildcmd (settings :: Settings)) ++ " 2>&1") {cwd = Just $ unpack $ directory settings}) ""
            TIO.putStrLn build
            pure build
          )

      let realcmd = "/bin/bash -c \"" <> "{ time " <> execcmd settings <> " 2>/dev/null; } 2>&1" <> "\""
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

numToText2 :: Double -> Text
numToText2 = printf "%.2f" >>> pack

withTailLf :: Text -> Text
withTailLf t =
  case T.unsnoc t of
    Just (_,'\n') -> t
    _             -> t <> "\n"

embed :: [(SourceFile, Text)] -> [BenchResult] -> Text -> Text
embed files results = embedIndex >>> embedResult files results

embedIndex :: Text -> Text
embedIndex text =
  let anchortag t    = "<a name='" <> t <> "'></a>"
      anchoridl2 i   = "anchor" <> tshow i
      anchoridl3 i j = "anchor" <> tshow i <> "-" <> tshow j
      (sections, anchored)
        = ( let self = fst $ T.breakOn "\n## " text
            in
            T.breakOnAll "\n## " >>>
            L.foldl' (\(i,xs,t) (before, after) ->
                let (inner,outer)        = T.breakOn "\n## " (T.drop 4 after)
                    title                = fst $ T.breakOn "\n" inner
                    self                 = fst $ T.breakOn "\n### " inner
                    (sections, anchored) =
                        (T.breakOnAll "\n### " >>>
                        L.foldl' (\(j,xs,t) (before, after) ->
                          let title    = fst $ T.breakOn "\n" $ T.drop 5 after
                              anchored = fst $ T.breakOn "\n### " (T.drop 5 after)
                          in (j+1, (title,[]):xs, t <> "\n### " <> anchortag (anchoridl3 i j) <> anchored)
                        ) (0,[],self) >>>
                        (\(j,xs,t) -> (reverse xs, t))
                        ) inner
                in (i+1, (title, sections):xs, t <> "\n## " <> anchortag (anchoridl2 i) <> anchored)
              ) (0,[],self) >>>
            (\(j,xs,t) -> (reverse xs, t))
          ) text
      indexmd = "## 目次\n" <>
                T.intercalate "\n" (mapWithIndex (\i s ->
                    "- <a href='#"<> anchoridl2 i <>"'>" <> fst s <> "</a>\n" <> T.intercalate "" (mapWithIndex (\j s ->
                    "  - <a href='#"<> anchoridl3 i j <>"'>" <> fst s <> "</a>\n"
                  ) (snd s))) sections)
  in replace "{index}" indexmd anchored

embedResult :: [(SourceFile, Text)] -> [BenchResult] -> Text -> Text
embedResult files results =
    flip (L.foldl' (\t file ->
      let fid  = (fst >>> fileid  ) file
          lang = (fst >>> filelang) file
          code = "\ncode:\n```" <> lang <> "\n" <> withTailLf (snd file) <> "```\n"
      in replace ("{file:"   <> fid <> "}") code t
    )) files >>>
    flip (L.foldl' (\t result ->
      let langid = (settings >>> settingsid) result
          lang   = (settings >>> directory) result
          code   = "\ncode:\n```" <> lang <> "\n" <> withTailLf (sourcestr result) <> "```\n"
          run    = "\nresult:\n```\n" <>
                    "$ " <> buildcmd (result :: BenchResult) <> "\n" <>
                    "$ " <> benchcmd result <> "\n" <> withTailLf (benchresult result) <> "```\n"
      in (replace ("{code:"   <> langid <> "}") code >>>
          replace ("{result:" <> langid <> "}") run >>>
          replace ("{sample:" <> langid <> "}") (code <> run)
          ) t
    )) results >>>
    replace "{ranking}" (
            rankingStr results
        )

rankingStr results =
  let sortedByReal = (L.map (\res -> ((settings >>> langname) res, (timeresult >>> \(r,u,s) -> r) res)) >>> sortOn snd) results
      sortedByUser = (L.map (\res -> ((settings >>> langname) res, (timeresult >>> \(r,u,s) -> u) res)) >>> sortOn snd) results
      tocol fastest rank (lang, time) = "| " <> tshow (rank+1) <> " | " <> lang <> " | " <> numToText2 time <> " sec. |" <> numToText2 (time/fastest) <> "x |\n"
      header = "| rank | lang | time | ratio | \n| - | - | - | - |\n"
  in "実行時間：\n" <> (header <> T.intercalate "" (mapWithIndex (tocol $ snd $ head sortedByReal) sortedByReal)) <> "\n" <>
     "CPU時間：\n" <> (header <> T.intercalate "" (mapWithIndex (tocol $ snd $ head sortedByUser) sortedByUser))
