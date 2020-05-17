{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module TermGraph (
  IsoTDag(..),
  TDag(..),
  TermF,
  TVars,
  arbitraryIso,
  fromTermPermute,
  generateIsoOfSize,
  runTests,
  show',
  tDag'
) where
  import Prelude                hiding (lookup)

  import Data.Mod.Word                 (Mod,unMod)
  import GHC.TypeNats                  (KnownNat)
  import GHC.Generics                  (Generic)
  import Control.Monad                 (liftM)
  import Test.QuickCheck               (Arbitrary(..),Gen,Property,forAllProperties,generate,label,maxSuccess,quickCheckWithResult,shuffle,stdArgs,suchThat)
  import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)
  import Data.Tuple                    (fst,swap)

  import Data.Graph                    (Graph,Tree(..),Vertex,dff,edges,vertices)
  import Data.Array                    (array,bounds,elems,indices,listArray,(!))
  import Data.Permute                  (Permute,at,listPermute)

  import Data.Map                      (Map,fromList,insert,lookup,size)
  import Control.Monad.State           (State,runState,state)
  import Data.Maybe                    (fromJust,isNothing)

  --
  newtype TVars m = TVars { unTVars :: Mod m } deriving (Bounded,Enum,Eq,Generic,Num,Ord)

  instance KnownNat m => Arbitrary (TVars m) where
    arbitrary = liftM (TVars . fromIntegral . unMod) genericArbitrary

  instance KnownNat m => Show (TVars m) where
    show = show . unMod . unTVars

  instance KnownNat m => Read (TVars m) where
    readsPrec _ = map (swap . fmap (TVars . fromIntegral) . swap) . reads

  --
  data TermF a
    = Var a
    | Fun0
    | Fun1 (TermF a)
    | Fun2 (TermF a) (TermF a)
    | Fun3 (TermF a) (TermF a) (TermF a)
    deriving (Eq,Generic,Ord)

  prop_Read_Show_Term :: TermF (TVars 3) -> Property
  prop_Read_Show_Term t = label l p where
    l = "size = " ++ show (sizeTerm t)
    p = t == (read . show) t

  vars :: (Bounded a, Enum a) => TermF a -> [a]
  vars _ = enumFromTo minBound maxBound

  nvars :: (Bounded a, Enum a) => TermF a -> Int
  nvars = length . vars

  sizeTerm :: TermF a -> Int
  sizeTerm (Var _)      = 1
  sizeTerm  Fun0        = 1
  sizeTerm (Fun1 t)     = 1 + sizeTerm t
  sizeTerm (Fun2 t s)   = 1 + sizeTerm t + sizeTerm s
  sizeTerm (Fun3 t s r) = 1 + sizeTerm t + sizeTerm s + sizeTerm r

  instance Functor TermF where
    fmap f (Var i)      = Var (f i)
    fmap _  Fun0        = Fun0
    fmap f (Fun1 t)     = Fun1 (fmap f t)
    fmap f (Fun2 t s)   = Fun2 (fmap f t) (fmap f s)
    fmap f (Fun3 t s r) = Fun3 (fmap f t) (fmap f s) (fmap f r)

  instance Arbitrary a => Arbitrary (TermF a) where
    arbitrary = genericArbitrary

  instance Show a => Show (TermF a) where
    show (Var i)      = "x" ++ show i
    show  Fun0        = "c"
    show (Fun1 t)     = "f(" ++ show t ++ ")"
    show (Fun2 t s)   = "g(" ++ show t ++ "," ++ show s ++ ")"
    show (Fun3 t s r) = "h(" ++ show t ++ "," ++ show s ++ "," ++ show r ++ ")"

  instance Read a => Read (TermF a) where
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

  --
  data TDag a = TDag { dag :: Graph, root :: Vertex }

  prop_TDag_dff :: TermF (TVars 3) -> Property
  prop_TDag_dff t = label l p where
    l = "size = " ++ show (sizeTerm t)
    tdag = toTDag t
    root' (Node r _) = r
    p = elem (root tdag) (map root' $ dff $ dag tdag)

  sizeTDag :: TDag a -> (Int,Int)
  sizeTDag tdag = (length $ vertices g, length $ edges g) where
    g = dag tdag
  
  show' :: TDag a -> String
  show' tdag = foldr1 (++) [
      show min, "\n",
      show max, "\n",
      foldr (++) "" [ show (length vs) ++ f vs ++ "\n" | vs <- elems g ]
    ] where g = dag tdag
            (min,max) = bounds g
            f = foldr (++) "" . map ((++) " " . show)

  instance Show (TDag a) where
    show tdag = v ++ es where
      v = "; " ++ show (root tdag) ++ "\n"
      es = foldr (++) "" [ show i ++ " -> " ++ show j ++ "\n" | (i,j) <- edges $ dag tdag ]

  --
  -- phi :: IsoTDag and f = iso phi: representing a pair of
  -- isomorphic term dags s.t. f (tDag phi) = tDag' phi
  --
  data IsoTDag a = IsoTDag { term :: TermF a, tDag :: TDag a, iso :: Permute }

  prop_IsoTDag :: IsoTDag (TVars 3) -> Property
  prop_IsoTDag phi = label l p where
    l = "size = " ++ show (sizeIsoTDag phi)
    p = (toTerm $ tDag phi) == (toTerm $ tDag' phi)

  tDag' :: IsoTDag a -> TDag a
  tDag' phi = TDag { dag = g', root = f $ root $ tDag phi } where
    g = dag $ tDag phi
    f = at $ iso phi
    idx = map f (indices g)
    elm = map (map f) (elems g)
    g' = array (bounds g) (zip idx elm)

  sizeIsoTDag = sizeTDag . tDag

  fromTermPermute :: (Bounded a, Enum a, Ord a) => TermF a -> Permute -> IsoTDag a
  fromTermPermute t p = IsoTDag { term = t, tDag = toTDag t, iso = p }

  arbitraryIso :: (Bounded a, Enum a, Ord a) => TermF a -> Gen (IsoTDag a)
  arbitraryIso t = do
    let (_,m) = bounds $ dag $ toTDag t
    l <- shuffle [nvars t .. m]
    let p = listPermute (m + 1) $ map fromEnum (vars t) ++ l
    return $ fromTermPermute t p

  generateIsoOfSize :: (Arbitrary a, Bounded a, Enum a, Ord a) => (Int,Int) -> IO (IsoTDag a)
  generateIsoOfSize (minV,minE) = generate $ suchThat arbitrary $ \phi -> let
    (v,e) = sizeIsoTDag phi
    in v >= minV && e >= minE

  instance Show a => Show (IsoTDag a) where
    show phi = "; " ++ (show $ term phi) ++ "\n" ++ (show $ tDag' phi)

  instance (Arbitrary a, Bounded a, Enum a, Ord a) => Arbitrary (IsoTDag a) where
    arbitrary = arbitrary >>= arbitraryIso

  --
  data TGState a = TGState {
      dict :: Map (TermF a) Vertex,
      grph :: [[Vertex]]
    }

  initTGState :: (Bounded a, Enum a, Ord a) => TermF a -> TGState a
  initTGState t = TGState {
      dict = fromList $ zip (map Var $ vars t) (map fromEnum $ vars t),
      grph = replicate (nvars t) []
    }

  lookupTermM :: Ord a => TermF a -> State (TGState a) (Maybe Vertex)
  lookupTermM t = state $ \s -> (lookup t $ dict s, s)

  insertTermM :: Ord a => TermF a -> [Vertex] -> State (TGState a) Vertex
  insertTermM t vs = state $ \s -> let
    v = size $ dict s
    dict' = insert t v $ dict s
    grph' = vs:(grph s)
    in (v, TGState { dict = dict', grph = grph' })

  --
  toArrayM :: Ord a => TermF a -> State (TGState a) Vertex
  toArrayM t = do
    m <- lookupTermM t
    case m of
      Just v -> return v
      Nothing -> case t of
        Fun3 s r q -> do { v <- toArrayM s ; u <- toArrayM r ; w <- toArrayM q ; insertTermM t [v,u,w] }
        Fun2 s r   -> do { v <- toArrayM s ; u <- toArrayM r ;                   insertTermM t [v,u] }
        Fun1 s     -> do { v <- toArrayM s ;                                     insertTermM t [v] }
        _          ->                                                            insertTermM t []

  toTDag :: (Bounded a, Enum a, Ord a) => TermF a -> TDag a
  toTDag t = TDag { dag = g, root = v } where
    (v,s) = runState (toArrayM t) (initTGState t)
    g = listArray (0, length (grph s) - 1) (reverse $ grph s)

  --
  toTermM :: (Bounded a, Enum a) => Graph -> Vertex -> Maybe (TermF a)
  toTermM g v = let
    ms = map (toTermM g) $ g ! v
    in if any isNothing ms then Nothing else let
      ts = map fromJust ms
      in case (length ts) of
        0 -> Just $ let t = if v < nvars t then Var $ toEnum v else Fun0 in t
        1 -> let [t]     = ts in Just $ Fun1 t
        2 -> let [t,s]   = ts in Just $ Fun2 t s
        3 -> let [t,s,r] = ts in Just $ Fun3 t s r
        _ -> Nothing

  toTerm :: (Bounded a, Enum a) => TDag a -> Maybe (TermF a)
  toTerm tdag = toTermM (dag tdag) (root tdag)

  prop_toTerm_isaRetractOf_toTDag :: TermF (TVars 3) -> Property
  prop_toTerm_isaRetractOf_toTDag t = label l p where
    l = "size = " ++ show (sizeTerm t)
    p = Just t == (toTerm . toTDag) t

  --
  return []
  runTests = $forAllProperties (quickCheckWithResult $ stdArgs { maxSuccess = 10000 })
