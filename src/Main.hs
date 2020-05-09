module Main where
  import TermGraph

  import System.Environment            (getArgs,getProgName)
  import System.Exit                   (ExitCode(..),exitWith)

  import Network.FastCGI               (CGI,CGIResult,getInput,handleErrors,liftIO,output,runFastCGI)
  import Text.XHtml             hiding (size)

  import Control.Monad                 (liftM)
  import Data.Maybe                    (fromJust,fromMaybe,isNothing)
  import Text.Read                     (readMaybe,reads)
  import Data.Permute                  (Permute,elems,indexOf,listPermute,permute,size)
  import Test.QuickCheck               (generate)
  import Data.Graph                    (edges,topSort,vertices)
  import Data.List                     (filter,length,null,sort)
  import Data.Tuple                    (fst)

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
    t <- liftM (>>=readMaybe) $ getInput "t"
    i <- liftM (>>=readMaybe) $ getInput "i"
    o <- liftM (>>=readMaybe) $ getInput "o"
    phi <- liftIO $ case (t,i) of
      (Just t', Just i') -> return $ fromTermPermute t' i'
      (Just t', Nothing) -> generate $ arbitraryIso t'
      (Nothing, _)       -> generateIsoOfSize (v,e)
    output $ renderHtml $
      header << thetitle << "Generate random term graphs" +++
      body << concatHtml [
        h1 << "Target graph size:",
        form << [
            p << ((label << "Minimum number of vertices: ") ! [strAttr "for" "v"]
                  +++ widget "number" "v" [intAttr "min" 1, intAttr "max" 99, intAttr "value" v]),
            p << ((label << "Minimum number of edges: ") ! [strAttr "for" "e"]
                  +++ widget "number" "e" [intAttr "min" 1, intAttr "max" 99, intAttr "value" e]),
            p << submit "" "Generate a new term"
          ],
        hr,
        h1 << "Term:",
        (textarea << show (term phi))
          ! [intAttr "rows"  4, intAttr "cols" 80, strAttr "name" "t", strAttr "form" "term-form"],
        p << (form << [submit "" "Generate an isomorphic graph",
                       hidden "v" (show v),
                       hidden "e" (show e)]) ! [strAttr "id" "term-form"],
        hr,
        h1 << "Graph:",
        (textarea << show phi)
          ! [intAttr "rows" 20, intAttr "cols" 40, emptyAttr "readonly"],
        (textarea << show' (termDag' phi))
          ! [intAttr "rows" 20, intAttr "cols" 40, emptyAttr "readonly"],
        hr,
        h1 << "Vertex list:",
        (textarea << if isNothing o then (show $ topSort $ getGraph $ termDag' phi) else (show $ elems $ fromJust o))
          ! [intAttr "rows"  4, intAttr "cols" 80, strAttr "name" "o", strAttr "form" "topsort-form"],
        p << (form << [submit "" "Is this a topological sort?",
                       hidden "v" (show v),
                       hidden "e" (show e),
                       hidden "t" (show $ term phi),
                       hidden "i" (show $ elems $ iso phi)]) ! [strAttr "id" "topsort-form"],
        p << if isNothing o then ""
        else if (size $ fromJust o) /= (length $ vertices $ getGraph $ termDag' phi) then "No, because of length mismatch"
        else let
          f = indexOf $ fromJust o
          es = filter (\(u,v) -> f u > f v) $ edges $ getGraph $ termDag' phi
          in if null es then "Yes" else "No, because of edge(s) " ++ show es
      ]

  instance Read Permute where
    readsPrec _ s = let
      isaPermutation xs = sort xs == [0 .. length xs - 1]
      in [ (listPermute (length l) l, s') | (l, s') <- filter (isaPermutation . fst) (reads s) ]
