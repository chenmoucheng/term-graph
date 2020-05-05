{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Main where
  import Prelude hiding (lookup)

  import GHC.TypeNats                  (natVal)
  import Data.Mod.Word                 (Mod,unMod)
  import Data.Tuple                    (fst,snd,swap)

  -- Copied from http://hackage.haskell.org/package/quickcheck-arbitrary-adt-0.3.1.0/docs/Test-QuickCheck-Arbitrary-ADT.html
  import Data.Proxy                    (Proxy(..))
  import GHC.Generics                  (Generic)
  import Test.QuickCheck as QC         (Arbitrary(..),forAllProperties,Gen,generate,label,maxSuccess,quickCheckWithResult,shuffle,stdArgs)
  import Test.QuickCheck.Arbitrary.ADT (genericArbitrary,ToADTArbitrary)

  import Control.Monad                 (liftM)

  import Data.Graph                    (dff,edges,Graph,Tree(..),Vertex,vertices)
  import Data.Array as Array           (array,bounds,elems,indices,listArray,(!))
  import Data.Permute                  (at,listPermute,Permute)

  import Data.Map                      (fromList,insert,lookup,Map,size)
  import Control.Monad.State           (runState,State,state)
  import Data.Maybe                    (fromJust,fromMaybe,isNothing)

  import System.Environment            (getArgs,getProgName)
  import System.Exit                   (ExitCode(..),exitWith)

  import Network.FastCGI               (CGI,CGIResult,getInput,handleErrors,liftIO,output,runFastCGI)
  import Text.XHtml as HTML     hiding (size)
  import Text.Read                     (readMaybe)

  --
  type NVars = 3
  nvars :: Int
  nvars = fromInteger $ toInteger $ natVal (Proxy :: Proxy NVars)

  fromInt :: Int -> Mod NVars
  fromInt = fromInteger . toInteger

  toInt :: Mod NVars -> Int
  toInt = fromInteger . toInteger . unMod

  instance Read (Mod NVars) where
    readsPrec _ = map (swap . fmap fromInt . swap) . reads

  instance Arbitrary (Mod NVars) where
    arbitrary = liftM (fromInteger . toInteger . unMod) genericArbitrary

  --
  data Term
    = Var (Mod NVars)
    | Fun0
    | Fun1 Term
    | Fun2 Term Term
    | Fun3 Term Term Term
    deriving (Eq,Generic,Ord)

  prop_Read_Show_Term t = QC.label l p where
    l = "size = " ++ show (sizeTerm t)
    p = t == (read . show) t

  sizeTerm :: Term -> Int
  sizeTerm (Var _)      = 1
  sizeTerm  Fun0        = 1
  sizeTerm (Fun1 t)     = 1 + sizeTerm t
  sizeTerm (Fun2 t s)   = 1 + sizeTerm t + sizeTerm s
  sizeTerm (Fun3 t s r) = 1 + sizeTerm t + sizeTerm s + sizeTerm r

  instance Show Term where
    show (Var i)      = "x" ++ show (unMod i)
    show  Fun0        = "c"
    show (Fun1 t)     = "f(" ++ show t ++ ")"
    show (Fun2 t s)   = "g(" ++ show t ++ "," ++ show s ++ ")"
    show (Fun3 t s r) = "h(" ++ show t ++ "," ++ show s ++ "," ++ show r ++ ")"

  instance Read Term where
    readsPrec p str0    =
      [ (Var i, str2) | ('x':str1, str2) <-         lex str0,
                               (i,   "") <-       reads str1 ] ++
      [ (Fun0,       str1) | ("c", str1) <-         lex str0 ] ++
      [ (Fun1 t,     str4) | ("f", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                             (")", str4) <-         lex str3 ] ++
      [ (Fun2 t s,   str6) | ("g", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                             (",", str4) <-         lex str3,
                               (s, str5) <- readsPrec p str4,
                             (")", str6) <-         lex str5 ] ++
      [ (Fun3 t s r, str8) | ("h", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                             (",", str4) <-         lex str3,
                               (s, str5) <- readsPrec p str4,
                             (",", str6) <-         lex str5,
                               (r, str7) <- readsPrec p str6,
                             (")", str8) <-         lex str7 ]

  instance Arbitrary Term where
    arbitrary = genericArbitrary

  instance ToADTArbitrary Term

  --
  newtype TermDag = TermDag { unTermDag :: (Graph,Vertex) }

  prop_TermDag_dff t = QC.label l p where
    l = "size = " ++ show (sizeTerm t)
    p = elem v (map root $ dff g) where
      (g,v) = unTermDag $ toTermDag t
      root (Node r _) = r

  instance Show TermDag where
    show tdag = v' ++ es where
      (g,v) = unTermDag tdag
      v' = "; " ++ show v ++ "\n"
      es = foldr (++) "" [ show i ++ " -> " ++ show j ++ "\n" | (i,j) <- edges g ]

  --
  -- phi :: IsoTermDag and f = iso phi: representing a pair of
  -- isomorphic term dags s.t. f (termDag phi) = termDag' phi
  --
  data IsoTermDag = IsoTermDag { term :: Term, termDag :: TermDag, iso :: Permute }

  termDag' :: IsoTermDag -> TermDag
  termDag' phi = TermDag (g',v') where
    (g,v) = unTermDag $ termDag phi
    f = at $ iso phi
    idx = map f (indices g)
    elm = map (map f) (elems g)
    g' = array (bounds g) (zip idx elm)
    v' = f v

  prop_IsoTermDag phi = QC.label l p where
    l = "size = " ++ show (sizeIsoTermDag phi)
    p = (toTerm $ termDag phi) == (toTerm $ termDag' phi)

  sizeIsoTermDag = (+) 1 . snd . bounds . fst . unTermDag . termDag

  arbitraryIso :: Term -> Gen IsoTermDag
  arbitraryIso t = do
    let tdag = toTermDag t
    let (_,m) = bounds $ fst $ unTermDag tdag
    l <- shuffle [nvars .. m]
    let p = listPermute (m + 1) ([0 .. nvars - 1] ++ l)
    return $ IsoTermDag { term = t, termDag = tdag, iso = p }

  generateIsoOfSize :: (Int,Int) -> Int -> IO IsoTermDag
  generateIsoOfSize (nV,nE) cnt = do
    phi <- generate (arbitrary :: Gen IsoTermDag)
    let (g,v) = unTermDag $ termDag phi
    let dV = abs $ nV - length (vertices g)
    let dE = abs $ nE - length (edges g)
    if (cnt < 1) || (dV < 2 && dE < 4)
      then return phi
      else generateIsoOfSize (nV,nE) (cnt - 1)

  instance Show IsoTermDag where
    show phi = "; " ++ (show $ term phi) ++ "\n" ++ (show $ termDag' phi)

  instance Arbitrary IsoTermDag where
    arbitrary = arbitrary >>= arbitraryIso

  --
  data TGState = TGState {
      dict :: Map Term Vertex,
      grph :: [[Vertex]]
    }

  initTGState :: TGState
  initTGState = TGState {
      dict = fromList [ (Var $ fromInt i, i) | i <- [0 .. nvars - 1] ],
      grph = replicate nvars []
    }

  lookupTermM :: Term -> State TGState (Maybe Vertex)
  lookupTermM t = state $ \s -> (lookup t (dict s), s)

  insertTermM :: Term -> [Vertex] -> State TGState Vertex
  insertTermM t vs = state $ \s -> let
    v = size $ dict s
    dict' = insert t v (dict s)
    grph' = vs:(grph s)
    in (v, TGState { dict = dict', grph = grph' })

  --
  toArrayM :: Term -> State TGState Vertex
  toArrayM t = do
    m <- lookupTermM t
    case m of
      Just v -> return v
      Nothing -> case t of
        Fun3 s r q -> do { v <- toArrayM s ; u <- toArrayM r ; w <- toArrayM q ; insertTermM t [v,u,w] }
        Fun2 s r   -> do { v <- toArrayM s ; u <- toArrayM r ;                   insertTermM t [v,u] }
        Fun1 s     -> do { v <- toArrayM s ;                                     insertTermM t [v] }
        _          ->                                                            insertTermM t []

  toTermDag :: Term -> TermDag
  toTermDag t = TermDag (g,v) where
    (v,s) = runState (toArrayM t) initTGState
    g = listArray (0, length (grph s) - 1) (reverse $ grph s)

  --
  toTermM :: Graph -> Vertex -> Maybe Term
  toTermM g v = let
    ms = map (toTermM g) (g Array.! v)
    in if any isNothing ms then Nothing else let
      ts = map fromJust ms
      in case (length ts) of
        0 ->                     Just $ if v < nvars then Var $ fromInt v else Fun0
        1 -> let [t]     = ts in Just $ Fun1 t
        2 -> let [t,s]   = ts in Just $ Fun2 t s
        3 -> let [t,s,r] = ts in Just $ Fun3 t s r
        _ -> Nothing

  toTerm :: TermDag -> Term
  toTerm = fromJust . uncurry toTermM . unTermDag 

  prop_toTerm_isaRetractOf_toTermDag t = QC.label l p where
    l = "size = " ++ show (sizeTerm t)
    p = t == (toTerm . toTermDag) t

  --
  return []

  main :: IO ()
  main = getArgs >>= parse >> (runFastCGI $ handleErrors cgiMain)

  parse []     = return ()
  parse ["-t"] = $forAllProperties (quickCheckWithResult $ stdArgs { maxSuccess = 10000 }) >> exitWith ExitSuccess
  parse _      = getProgName >>= usage >> exitWith ExitSuccess

  usage progName = putStrLn $ "Usage: " ++ progName ++ " [-t]"

  cgiMain :: CGI CGIResult
  cgiMain = do
    v <- liftM (fromMaybe  8 . (>>=readMaybe)) $ getInput "v"
    e <- liftM (fromMaybe 16 . (>>=readMaybe)) $ getInput "e"
    t <- liftM                 (>>=readMaybe)  $ getInput "t"
    phi <- liftIO $ case t of
      Just t' -> generate $ arbitraryIso t'
      Nothing -> generateIsoOfSize (v,e) 100000
    output $ renderHtml $
      header << thetitle << "Generate a random term graph" +++
      body << concatHtml [
        h1 << "Term:",  (textarea << show (term phi)) HTML.! [intAttr "rows"  4, intAttr "cols" 80, strAttr "name" "t", strAttr "form" "term-form"],
        h1 << "Graph:", (textarea << show       phi)  HTML.! [intAttr "rows" 40, intAttr "cols" 80],
        p << (form << [submit "" "Generate an isomorphic graph", hidden "v" (show v), hidden "e" (show e)]) HTML.! [strAttr "id" "term-form"], hr,
        form << [
            p << ((HTML.label << "Number of vertices: ") HTML.! [strAttr "for" "v"] +++ widget "number" "v" [intAttr "min" 1, intAttr "max" 99, intAttr "value" v]),
            p << ((HTML.label << "Number of edges: ")    HTML.! [strAttr "for" "e"] +++ widget "number" "e" [intAttr "min" 1, intAttr "max" 99, intAttr "value" e]),
            p << submit "" "Generate a new term graph"
          ]
      ]
