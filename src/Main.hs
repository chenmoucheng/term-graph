module Main where
  import TermGraph

  import System.Environment            (getArgs,getProgName)
  import System.Exit                   (ExitCode(..),exitWith)

  import Control.Monad                 (liftM)
  import Data.Maybe                    (fromMaybe)
  import Test.QuickCheck               (generate)

  import Network.FastCGI               (CGI,CGIResult,getInput,handleErrors,liftIO,output,runFastCGI)
  import Text.XHtml as HTML     hiding (size)
  import Text.Read                     (readMaybe)

  --
  main :: IO ()
  main = getArgs >>= parse >> (runFastCGI $ handleErrors cgiMain)

  parse []     = return ()
  parse ["-t"] = runTests >> exitWith ExitSuccess
  parse _      = getProgName >>= usage >> exitWith ExitSuccess

  usage progName = putStrLn $ "Usage: " ++ progName ++ " [-t]"

  cgiMain :: CGI CGIResult
  cgiMain = do
    v <- liftM (fromMaybe  8 . (>>=readMaybe)) $ getInput "v"
    e <- liftM (fromMaybe 16 . (>>=readMaybe)) $ getInput "e"
    t <- liftM                 (>>=readMaybe)  $ getInput "t"
    phi <- liftIO $ case t of
      Just t' -> generate $ arbitraryIso t'
      Nothing -> generateIsoOfSize (v,e)
    output $ renderHtml $
      header << thetitle << "Generate a random term graph" +++
      body << concatHtml [
        h1 << "Term:",  (textarea << show      (term phi)) HTML.! [intAttr "rows"  4, intAttr "cols" 80, strAttr "name" "t", strAttr "form" "term-form"],
        h1 << "Graph:", (textarea << show            phi)  HTML.! [intAttr "rows" 40, intAttr "cols" 80],
                        (textarea << show' (termDag' phi)) HTML.! [intAttr "rows" 40, intAttr "cols" 80],
        p << (form << [submit "" "Generate an isomorphic graph", hidden "v" (show v), hidden "e" (show e)]) HTML.! [strAttr "id" "term-form"], hr,
        form << [
            p << ((HTML.label << "Minimum number of vertices: ") HTML.! [strAttr "for" "v"] +++ widget "number" "v" [intAttr "min" 1, intAttr "max" 99, intAttr "value" v]),
            p << ((HTML.label << "Minimum number of edges: ")    HTML.! [strAttr "for" "e"] +++ widget "number" "e" [intAttr "min" 1, intAttr "max" 99, intAttr "value" e]),
            p << submit "" "Generate a new term graph"
          ]
      ]
