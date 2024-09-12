{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Generic encoding of transition systems
-- not related to a particular case study

{-# LANGUAGE  ExistentialQuantification #-}
module TransitionSystem (printAut, currAut)  where

import Data.List ( intersperse )
import Data.Map as M hiding (filter, foldr, map)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (runGraphviz, graphToDot, quickParams, Labellable)
import Data.GraphViz.Commands (GraphvizOutput(..))
import Prettyprinter

newtype Loc = Loc {nameOfLoc :: String}
  deriving (Eq, Ord, Show, Read)


type Name = String  -- TODO: is Text in Simala

type Var = Name
type ChannelName = Name

-- copied from Simala
data Lit =
    IntLit     Int
  | BoolLit    Bool           -- could also be done using two nullary built-ins
  deriving (Eq, Ord, Show, Read)

-- partially copied from Simala
data Builtin =
    Minus      -- ^ subtraction, arity 2
  | Sum        -- ^ sum, flexible arity
  | Product    -- ^ product, flexible arity
  | Not        -- ^ logical negation, arity 1
  | Lt         -- ^ less-than, arity 2
  | Le         -- ^ less-than-or-equal, arity 2
  | Gt         -- ^ greater-than, arity 2
  | Ge         -- ^ greater-than-or-equal, arity 2
  | Eq         -- ^ equality (of Booleans, numbers or atoms), arity 2
  | Ne         -- ^ inequality (of Booleans, numbers or atoms), arity 2
  | And        -- ^ logical and, flexible arity
  | Or         -- ^ logical or, flexible arity
  deriving (Eq, Ord, Show, Read)

-- partially copied from Simala
data Expr =
    Builtin    Builtin [Expr]
  | Var        Name
  | Lit        Lit
  deriving (Eq, Ord, Show, Read)

-- some abbreviations

il :: Int -> Expr
il = Lit . IntLit

bl :: Bool -> Expr
bl = Lit . BoolLit

minus :: Expr -> Expr -> Expr
minus e1 e2 = Builtin Minus [e1, e2]

plus :: Expr -> Expr -> Expr
plus e1 e2 = Builtin Sum [e1, e2]

gt :: Expr -> Expr -> Expr
gt e1 e2 = Builtin Gt [e1, e2]

le :: Expr -> Expr -> Expr
le e1 e2 = Builtin Le [e1, e2]

data Cmd = VAssign Var Expr
  deriving (Eq, Ord, Show, Read)

type TransitionAction = [Cmd]

type TransitionGuard = Expr

data Sync
  = NoSync                     -- sometimes called tau or epsilon transition
  | SendSync ChannelName       -- c!
  | RecvSync ChannelName       -- c?
  deriving (Eq, Ord, Show, Read)

isNoSync :: Sync -> Bool
isNoSync NoSync = True
isNoSync _ = False

isSendSync :: Sync -> Bool
isSendSync (SendSync _) = True
isSendSync _ = False

isRecvSync :: Sync -> Bool
isRecvSync (RecvSync _) = True
isRecvSync _ = False



data Transition l = Transition {
    sourceOfTransition :: l
  , guardOfTransition :: TransitionGuard
  , syncOfTransition :: Sync
  , actionOfTransition :: TransitionAction
  , targetOfTransition :: l
  }
  deriving (Eq, Ord, Show, Read)


data Aut l =
  Aut {
      nameOfAut :: String
    , locsOfAut :: [l]
    , transitionsOfAut :: [Transition l]
    , initialLocOfAut :: l
  }
  deriving (Eq, Ord, Show, Read)

data Sys l = Sys {
    declsOfSys :: [Var]
  , channelsOfSys :: [ChannelName]
  , automataOfSys :: [Aut l]
  }
  deriving (Eq, Ord, Show, Read)


transitionFromHere :: Eq l =>  Transition l -> l -> Bool
transitionFromHere tr loc = sourceOfTransition tr == loc


listProd :: [[a]] -> [[a]]
listProd =
  let prod xs yss = [ x:ys | x <- xs, ys <- yss ]
  in foldr prod [[]]

-- Tagging transitions as external or internal
-- Internal transitions are added during product automata construction
-- to model no-action transitions from each location to itself,
-- allowing a component automaton to remain in the same location.
-- Internal transitions are always NoSync, external transitions can be either.
data Visibility t = External t | Internal t
  deriving (Eq, Ord, Show, Read)

-- One (ProdLoc ls), with ls a list of locations, corresponds to one state of the product automaton
-- The constructor ProdLoc is added for better visibility
newtype ProdLoc l = ProdLoc [l]
  deriving (Eq, Ord, Show, Read)

-- A ProdTrans is a transition in the product automaton emanating from a ProdLoc
newtype ProdTrans l = ProdTrans [Visibility (Transition l)]
  deriving (Eq, Ord, Show, Read)

noActTrans :: [l] -> [Visibility (Transition l)]
noActTrans = map (\l -> Internal $ Transition l (bl True) NoSync [] l)

-- A transition that is either internal or otherwise only synced over c
transSyncedOverChannelOrInternal :: ChannelName -> Visibility (Transition l) -> Bool
transSyncedOverChannelOrInternal c (External tr) = syncOfTransition tr == SendSync c || syncOfTransition tr == RecvSync c
transSyncedOverChannelOrInternal _c (Internal _tr) = True

transExternal :: Visibility t -> Bool
transExternal (External _) = True
transExternal (Internal _) = False

transInternal :: Visibility t -> Bool
transInternal = not . transExternal

hideVisibility :: Visibility t -> t
hideVisibility (External t) = t
hideVisibility (Internal t) = t

prodTransFromHere :: Eq l => [ProdTrans l] -> ProdLoc l -> [ProdTrans l]
prodTransFromHere allTrs (ProdLoc ploc) =
  filter (\(ProdTrans trs) -> and (zipWith (transitionFromHere . hideVisibility) trs ploc)) allTrs

noSyncVisTrans :: Visibility (Transition l) -> Bool
noSyncVisTrans = isNoSync . syncOfTransition . hideVisibility

-- A product transition containing exactly one NoSync external transition
-- A NoSync transition of two component automata should not be taken concurrently, but only sequentially
noSyncProdTrans :: ProdTrans l -> Bool
noSyncProdTrans (ProdTrans t) =
  let ext = filter transExternal t
  in length ext == 1 && all noSyncVisTrans ext

prodOfTrans :: (Sync, ProdTrans l) -> Transition [l]
prodOfTrans (sync, ProdTrans t) = Transition {
    sourceOfTransition = map (sourceOfTransition . hideVisibility) t
  , guardOfTransition = Builtin And (map (guardOfTransition . hideVisibility) t)
  , syncOfTransition = sync
  , actionOfTransition = concatMap (actionOfTransition . hideVisibility) t
  , targetOfTransition = map (targetOfTransition . hideVisibility) t
}

-- a product transition is synchronized over the same channel
allSyncedOverChannel :: ChannelName -> ProdTrans l -> Bool
allSyncedOverChannel c (ProdTrans ts) =
  all (transSyncedOverChannelOrInternal c) ts
  && any (isSendSync . syncOfTransition . hideVisibility) ts

count :: (t -> Bool) -> [t] -> Int
count p xs = sum (map (\x -> if p x then 1 else 0) xs)

maximallySyncedOverChannel :: ChannelName -> [ProdTrans l] -> [ProdTrans l]
maximallySyncedOverChannel c prtrs =
  let allSynced = filter (allSyncedOverChannel c) prtrs
      externalCounts = map (\(ProdTrans tr) -> count transExternal tr) allSynced
      m = maximum externalCounts
  in filter (\(ProdTrans tr) -> count transExternal tr == m) allSynced


-- Assuming there are n automata, for automata [a1 .. an], 
-- allTrans is the list composed of lists [t1 .. tn] 
-- with ti stemming from ai; ploc is a product location [l1 .. ln]
getApplicableTransOfProdLoc :: Eq l => [ChannelName] -> [ProdTrans l] -> ProdLoc l -> [Transition [l]]
getApplicableTransOfProdLoc chs allTrs ploc =
  let allTransFromHere = prodTransFromHere allTrs ploc
      syncTr = concatMap (\c -> map (SendSync c, ) (maximallySyncedOverChannel c allTransFromHere) ) chs
      noSyncTr = filter noSyncProdTrans allTransFromHere
  in map prodOfTrans (syncTr ++ map (NoSync, ) noSyncTr)

filterApplicableTrans :: Eq l => [ChannelName] -> [ProdTrans l] -> [ProdLoc l] -> [Transition [l]]
filterApplicableTrans chs allTrs = concatMap (getApplicableTransOfProdLoc chs allTrs)

allTrans :: [Aut l] -> [[Visibility (Transition l)]]
allTrans = map (\a -> noActTrans (locsOfAut a) ++ map External (transitionsOfAut a))

-- TODO: also test for empty system (containing no automata) 
-- TODO: check for multiple assignments in action of transition after product
prodAut :: Eq l => Sys l -> Aut [l]
prodAut sys =
  let lp = listProd (map locsOfAut (automataOfSys sys))
      allTr = allTrans (automataOfSys sys)
      prodTr = map ProdTrans (listProd allTr)
      applicableTr = filterApplicableTrans (channelsOfSys sys) prodTr (map ProdLoc lp)
  in Aut {
      nameOfAut = nameProdAut (map nameOfAut (automataOfSys sys))
    , locsOfAut = lp
    , transitionsOfAut = applicableTr
    , initialLocOfAut = map initialLocOfAut (automataOfSys sys)
    }


----------------------------------------------------------------------------------------
-- Syntactic minimization: restriction to reachable locations
----------------------------------------------------------------------------------------

transclos :: Ord a => (a -> S.Set a) -> S.Set a -> S.Set a
transclos f xs =
  let next = S.union xs (S.unions (S.map f xs))
  in if S.isSubsetOf next xs then next else transclos f next

successors :: Ord l => [Transition l] -> l -> S.Set l
successors trs l = S.fromList [targetOfTransition t | t <- trs, transitionFromHere t l]

restrictTrans :: Ord l => [Transition l] -> S.Set l -> [Transition l]
restrictTrans trs reach = [t | t <- trs, S.member (sourceOfTransition t) reach]

minAut :: Ord l => Aut l -> Aut l
minAut aut =
  let reach = transclos (successors (transitionsOfAut aut)) (S.singleton (initialLocOfAut aut))
  in Aut (nameOfAut aut) (S.toList reach) (restrictTrans (transitionsOfAut aut) reach) (initialLocOfAut aut)

----------------------------------------------------------------------------------------
-- Semantics: Running automata
----------------------------------------------------------------------------------------
-- Semantics of running automata, and executing an automaton until a condition is satisfied.
-- Unassigned variables are handled in a somewhat lax manner, see functions variableHasIntValS etc.

data Val
  = BoolVal Bool
  | IntVal Int
  deriving (Eq, Ord, Show, Read)


-- State: a location and associated variable assignments
data State l = State {
    currLoc :: l
  , assignments :: Map Var Val
  }
  deriving (Eq, Ord, Show, Read)


type FRStateContract l a b = (State l, a) -> Either String (State l, b)

valOfLit :: Lit -> Val
valOfLit (IntLit i) = IntVal i
valOfLit (BoolLit b) = BoolVal b

liftIntIntToInt :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntIntToInt op (IntVal i1) (IntVal i2) = IntVal (op i1 i2)
liftIntIntToInt _ _ _ = error "internal: liftIntIntToInt"

liftBoolBoolToBool :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolBoolToBool op (BoolVal b1) (BoolVal b2) = BoolVal (op b1 b2)
liftBoolBoolToBool _ _ _ = error "internal: liftIntIntToInt"

liftIntIntToBool :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftIntIntToBool op (IntVal i1) (IntVal i2) = BoolVal (op i1 i2)
liftIntIntToBool _ _ _ = error "internal: liftIntIntToBool"

liftBoolToBool :: (Bool -> Bool) -> Val -> Val
liftBoolToBool op (BoolVal b) = BoolVal (op b)
liftBoolToBool _ _ = error "internal: liftBoolToBool"


evalExpr :: Map Var Val -> Expr -> Val
evalExpr assm (Var v) = fromJust (M.lookup v assm)
evalExpr _ (Lit l) = valOfLit l
evalExpr assm (Builtin Minus [e1, e2]) = liftIntIntToInt (-) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Sum es) = foldr (liftIntIntToInt (+) . evalExpr assm) (IntVal 0) es
evalExpr assm (Builtin Product es) = foldr (liftIntIntToInt (*) . evalExpr assm) (IntVal 1) es
evalExpr assm (Builtin Not [e]) = liftBoolToBool not (evalExpr assm e)
evalExpr assm (Builtin Lt [e1, e2]) = liftIntIntToBool (<) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Le [e1, e2]) = liftIntIntToBool (<=) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Gt [e1, e2]) = liftIntIntToBool (>) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Ge [e1, e2]) = liftIntIntToBool (>=) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Eq [e1, e2]) = liftIntIntToBool (==) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin Ne [e1, e2]) = liftIntIntToBool (/=) (evalExpr assm e1) (evalExpr assm e2)
evalExpr assm (Builtin And es) = foldr (liftBoolBoolToBool (&&) . evalExpr assm) (BoolVal True) es
evalExpr assm (Builtin Or es) = foldr (liftBoolBoolToBool (||) . evalExpr assm) (BoolVal False) es
evalExpr _assm _ = error "internal: evalExpr"


isTrue :: Val -> Bool
isTrue (BoolVal b) = b
isTrue _ = False

initialState :: Aut l -> State l
initialState a = State (initialLocOfAut a) M.empty

evalGuard :: Map Var Val -> TransitionGuard -> Val
evalGuard = evalExpr

enabledInState :: Transition l -> State l -> Bool
enabledInState tr s = isTrue (evalGuard (assignments s) (guardOfTransition tr))

execCmd :: Map Var Val -> Cmd -> Map Var Val
execCmd assm (VAssign vr e) = M.insert vr (evalExpr assm e) assm

execAction :: Map Var Val -> TransitionAction -> Map Var Val
execAction = Prelude.foldl execCmd

stepTransition :: State l -> Transition l -> (Transition l, State l)
stepTransition s tr = (tr, State (targetOfTransition tr) (execAction (assignments s) (actionOfTransition tr)))

stepAut :: Eq l => Aut l -> State l -> [(Transition l, State l)]
stepAut a s =
  let enabledTrs = [tr | tr <- transitionsOfAut a, transitionFromHere tr (currLoc s), enabledInState tr s]
  in map (stepTransition s) enabledTrs


-- Ideally, a trace is a list of states in reverse order, i.e. most recent state first, initial state last.
-- In order not to have to work with a non-empty list type, the most recent state is singled out.
-- The trace also records the transition applied, so a trace (s(n+1), [(tn, sn), ... (t0, s0)]
-- corresponds to s0 -t0-> s1 .. tn -> s(n+1)
type Trace l = (State l, [(Transition l, State l)])

initialTrace :: Aut l -> Trace l
initialTrace a = (initialState a, [])

stepTrace :: Eq l => Aut l -> Trace l -> [Trace l]
stepTrace a (s, tss) = map (\(t, sn) -> (sn, (t,s):tss)) (stepAut a s)

type SearchState l = [Trace l]

-- breadth first search
-- bfs :: (Trace l -> SearchState l) -> (Trace l -> Bool) -> SearchState l -> [Trace l]
bfs :: (t -> [t]) -> (t -> Bool) -> [t] -> [t]
bfs _next _sol [] = []
bfs next sol (s:ss) =
  if sol s
  then s: bfs next sol (ss ++ next s)
  else bfs next sol (ss ++ next s)

-- Run an automaton until a solution condition 'sol' on the traces is satisfied. 
-- Controlling the length of the trace is an indirect way of bounding the depth of the search.
runAut :: Eq l => Aut l -> (Trace l -> Bool) -> [Trace l]
runAut a sol = bfs (stepTrace a) sol [initialTrace a]


-- Check if an integer variable has a value (given by a predicate p) in a state respectively a trace.
-- Attention, the test returns False if the variable is not assigned
variableHasIntValS :: Var -> (Int -> Bool) -> State l -> Bool
variableHasIntValS v p s =
  let test vl = case vl of
                  IntVal i -> p i
                  _ -> False
  in maybe False test (M.lookup v (assignments s))

variableHasIntValTr :: Var -> (Int -> Bool) -> Trace l -> Bool
variableHasIntValTr v p (s, _) =  variableHasIntValS v p s


-- Check if a Boolean variable has a value in a state respectively a trace.
-- Attention, the test returns False if the variable is not assigned
variableHasBoolValS :: Var -> State l -> Bool
variableHasBoolValS v s =
  let test vl = case vl of
                  BoolVal b -> b
                  _ -> False
  in maybe False test (M.lookup v (assignments s))

variableHasBoolValTr :: Var -> Trace l -> Bool
variableHasBoolValTr v (s, _) =  variableHasBoolValS v s

isInLocS :: Eq l => l -> State l -> Bool
isInLocS l s = currLoc s == l

isInLocTr :: Eq l => l -> Trace l -> Bool
isInLocTr l (s, _) = isInLocS l s

-- >>> runAut autA (variableHasIntValTr "x" (>= 3))
-- [(State {currLoc = "l2A", assignments = fromList [("x",IntVal 3)]},[(Transition {sourceOfTransition = "l1A", guardOfTransition = Lit (BoolLit True), syncOfTransition = Just "c1", actionOfTransition = [VAssign "x" (Lit (IntLit 3))], targetOfTransition = "l2A"},State {currLoc = "l1A", assignments = fromList []})]),(State {currLoc = "l3A", assignments = fromList [("x",IntVal 4)]},[(Transition {sourceOfTransition = "l1A", guardOfTransition = Lit (BoolLit True), syncOfTransition = Nothing, actionOfTransition = [VAssign "x" (Lit (IntLit 4))], targetOfTransition = "l3A"},State {currLoc = "l1A", assignments = fromList []})])]


-- >>> runAut (minAut (prodAut sysABC)) (variableHasIntValTr "x" (== 3))
-- [(State {currLoc = ["l2A","l2B","l1C"], assignments = fromList [("x",IntVal 3)]},[(Transition {sourceOfTransition = ["l1A","l1B","l1C"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Just "c1", actionOfTransition = [VAssign "x" (Lit (IntLit 3))], targetOfTransition = ["l2A","l2B","l1C"]},State {currLoc = ["l1A","l1B","l1C"], assignments = fromList []})]),(State {currLoc = ["l2A","l2B","l2C"], assignments = fromList [("x",IntVal 3)]},[(Transition {sourceOfTransition = ["l1A","l1B","l2C"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Just "c1", actionOfTransition = [VAssign "x" (Lit (IntLit 3))], targetOfTransition = ["l2A","l2B","l2C"]},State {currLoc = ["l1A","l1B","l2C"], assignments = fromList [("x",IntVal 5)]}),(Transition {sourceOfTransition = ["l1A","l1B","l1C"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Nothing, actionOfTransition = [VAssign "x" (Lit (IntLit 5))], targetOfTransition = ["l1A","l1B","l2C"]},State {currLoc = ["l1A","l1B","l1C"], assignments = fromList []})])]


----------------------------------------------------------------------------------------
-- CTL*
----------------------------------------------------------------------------------------
-- Bounded concrete model search in CTL (see function runUpctlSearch):
-- Run a scenario up to a certain depth to check whether a CTL formula holds,
-- with possible results Valid | Invalid | Inconclusive
-- Note that this is not (abstract) CTL model checking
-- The function runAut is a restricted form of runUpctlSearch


-- for the time being, keep separate from the Boolean connectors And and Or
data Connector = LogAnd | LogOr
  deriving (Eq, Ord, Show, Read)

-- path quantifier: for all paths, there exists a path
data PQuantif = A | E
  deriving (Eq, Ord, Show, Read)

-- state quantifier: generally, finally, next
data SQuantif = G | F | N
  deriving (Eq, Ord, Show, Read)

-- State formulas
data SForm l
  = Basic (State l -> Bool)
  | SNeg (SForm l)
  | SBin Connector (SForm l) (SForm l)
  | SQuant PQuantif (PForm l)
--  deriving (Show, Read)

-- Path formulas
data PForm l
  = PState (SForm l)
  | PBin Connector (PForm l) (PForm l)
  | PQuant SQuantif (SForm l)
--  deriving (Show, Read)

-- CTLForm: the CTL* data types SForm and PForm contracted to CTL
data CTLForm b
  = CTLBasic b
  | CTLNeg (CTLForm b)
  | CTLBin Connector (CTLForm b) (CTLForm b)
  | CTLQuant PQuantif SQuantif (CTLForm b)
  deriving (Eq, Ord, Show, Read)

-- Simplified CTL formulas as used in Uppaal, 
-- consisting of a path/state quantifier prefix and a quantifier-free body
-- For more complex CTL formulas like (E<> P) && (E<> Q), 
-- it is not even clear what a trace might be
data UpCTLExpr b
  = UpCTLBasic b
  | UpCTLNeg (UpCTLExpr b)
  | UpCTLBin Connector (UpCTLExpr b) (UpCTLExpr b)

data UpCTLForm b
  = UpCTLQuant {
      uppquantif :: PQuantif
    , upsquantif :: SQuantif
    , upexpr :: UpCTLExpr b
  }

-- Proof tree node
data PTNodeS l = PTNodeS {
    ptState :: State l
  , ptSForm :: SForm l
  , ptsSuccs :: [PTNode l]
  , ptsJudgm :: Bool
  }

data PTNodeP l = PTNodeP {
    ptTrans :: Transition l
  , ptPForm :: PForm l
  , ptpSuccs :: [PTNode l]
  , ptpJudgm :: Bool
  }

data PTNode l = PTS (PTNodeS l) | PTP (PTNodeP l)


data UpCTLSearchState l = UpCTLSearchState {
    -- upctlForm :: UpCTLForm b
     upctlResult :: SearchResult
  ,  upctlTrace :: Trace l
}
  deriving (Eq, Ord, Show, Read)

data SearchResult = Valid | Invalid | Inconclusive
  deriving (Eq, Ord, Show, Read)


upform :: PQuantif -> SQuantif -> b -> UpCTLForm b
upform pq sq b = UpCTLQuant pq sq (UpCTLBasic b)

-- >>> runUpctlSearch autA 2 (upform A F (variableHasIntValS "x" (>= 3)))
-- UpCTLSearchState {upctlResult = Valid, upctlTrace = (State {currLoc = "l2A", assignments = fromList [("x",IntVal 3)]},[(Transition {sourceOfTransition = "l1A", guardOfTransition = Lit (BoolLit True), syncOfTransition = SendSync "c1", actionOfTransition = [VAssign "x" (Lit (IntLit 3))], targetOfTransition = "l2A"},State {currLoc = "l1A", assignments = fromList []})])}


-- >>> runUpctlSearch (minAut (prodAut sysABC)) 4 (upform A G (variableHasIntValS "x" (>= 3)))
-- UpCTLSearchState {upctlResult = Invalid, upctlTrace = (State {currLoc = ["l1A","l1B","l1C"], assignments = fromList []},[])}

-- run concrete model search for automaton a, up to depth d, checking formula f

runUpctlSearch :: Eq l => Aut l -> Int -> UpCTLForm (State l -> Bool) -> UpCTLSearchState l
runUpctlSearch a d f = upctlSearch a d f (initialTrace a)
upctlSearch :: Eq l => Aut l -> Int -> UpCTLForm (State l -> Bool) -> Trace l -> UpCTLSearchState l
upctlSearch a d form tr = genNexts a d form tr (evalUpCTLFormHere (fst tr) form)

genNexts :: Eq l => Aut l -> Int -> UpCTLForm (State l -> Bool) -> Trace l -> SearchResult -> UpCTLSearchState l
genNexts _a _d _form tr Valid = UpCTLSearchState Valid tr
genNexts _a _d _form tr Invalid = UpCTLSearchState Invalid tr
genNexts a d form tr Inconclusive =
  if d == 0
  then UpCTLSearchState Inconclusive tr
  else
    let searchres = map (upctlSearch a (d - 1) form) (stepTrace a tr)
        nexts = evalRes (uppquantif form) tr searchres
  in nexts


evalRes :: PQuantif -> Trace l-> [UpCTLSearchState l] -> UpCTLSearchState l
evalRes A tr res =
  case [ups | ups <- res, upctlResult ups == Invalid ] of
    [] -> (case [ups | ups <- res, upctlResult ups == Inconclusive ] of
            [] -> (case [ups | ups <- res, upctlResult ups == Valid] of
                    [] -> UpCTLSearchState Invalid tr
                    r : _  -> r)
            r : _  -> r)
    r : _  -> r

evalRes E tr res =
  case [ups | ups <- res, upctlResult ups == Valid ] of
    [] -> (case [ups | ups <- res, upctlResult ups == Inconclusive ] of
            [] -> (case [ups | ups <- res, upctlResult ups == Invalid] of
                    [] -> UpCTLSearchState Invalid tr
                    r : _  -> r)
            r : _  -> r)
    r : _  -> r


evalConn :: Connector -> Bool -> Bool -> Bool
evalConn LogAnd = (&&)
evalConn LogOr  = (||)

evalUpCTLExprHere :: State l -> UpCTLExpr (State l -> Bool) -> Bool
evalUpCTLExprHere s (UpCTLBasic b) = b s
evalUpCTLExprHere s (UpCTLNeg f) = not (evalUpCTLExprHere s f)
evalUpCTLExprHere s (UpCTLBin cn f1 f2) = evalConn cn (evalUpCTLExprHere s f1) (evalUpCTLExprHere s f2)

evalUpCTLFormHere :: State l -> UpCTLForm (State l -> Bool) -> SearchResult
evalUpCTLFormHere s (UpCTLQuant _pq G f) =
  if evalUpCTLExprHere s f then Inconclusive else Invalid
evalUpCTLFormHere s (UpCTLQuant _pq F f) =
  if evalUpCTLExprHere s f then Valid else Inconclusive
evalUpCTLFormHere _s (UpCTLQuant _pq N _f) = Inconclusive  -- the N constructor is not handled 



----------------------------------------------------------------------------------------
-- Visualization of automata
----------------------------------------------------------------------------------------

-- node index as required by the graph library
type NInd = Int

bidirLookup :: Eq a => [(a, b)] -> a -> b
bidirLookup xs a = fromJust $ Prelude.lookup a xs

edgeSetSymToInd :: Eq n => [(n, NInd)] -> (e -> n) -> (e -> n) -> (e -> ei) -> [e] -> [(NInd, NInd, ei)]
edgeSetSymToInd nodeSymInd src trg info = map (\e -> (bidirLookup nodeSymInd (src e), bidirLookup nodeSymInd (trg e), info e))

mkLabelledGraph :: Eq n => [n] -> [e] -> (e -> n) -> (e -> n) -> (n -> ni) -> (e -> ei) -> Gr ni ei
mkLabelledGraph nodelist edgelist src trg ninfo einfo =
  let nodeIndInfo = zip [0..length nodelist-1] (map ninfo nodelist)
      nodeSymInd = zip nodelist [0..length nodelist-1]
      gEdges = edgeSetSymToInd nodeSymInd src trg einfo edgelist
  in mkGraph nodeIndInfo gEdges

-- convert automaton to Dot and write to file "compactPropGraph.pdf".
-- ninfo: function for displaying nodes (typically: id for simple and nameProdAut for product automaton)
-- einfo: function for displaying edges (typically: edgeLabelAut for simple and product automaton)
autToDot :: (Eq l, Labellable ni, Labellable ei) => (l -> ni) -> (Transition l -> ei) -> Aut l -> IO FilePath
autToDot ninfo einfo aut = do
    let mypropgraph = mkLabelledGraph (locsOfAut aut) (transitionsOfAut aut) sourceOfTransition targetOfTransition ninfo einfo
    runGraphviz (graphToDot quickParams mypropgraph) Pdf "compactPropGraph.pdf"

nameProdAut :: [Name] -> Name
nameProdAut = foldr (<>)  "" . Data.List.intersperse "_"

edgeLabelAut :: Transition l -> String
edgeLabelAut tr =
  case syncOfTransition tr of
    NoSync -> ""
    SendSync c -> c ++ "!"
    RecvSync c -> c ++ "?"

-- >>> autToDot id edgeLabelAut autA
-- "compactPropGraph.pdf"

-- >>> autToDot nameProdAut edgeLabelAut (minAut (prodAut sysABC))
-- "compactPropGraph.pdf"

----------------------------------------------------------------------------------------
-- Conversion of automata to imperative programs for software model checking
----------------------------------------------------------------------------------------

-- Automata converted to a (kind of) C programs,
-- with the intention of verification by Ultimate LTL Automizer

-- Fundamental shortcomings:
-- * Alternative transitions of a location are sequentialized in code.
-- If the transition guards are not mutually exclusive, some transitions might be shadowed.
-- * LTL not adequate for reasoning about non-determinism

-- Practical shortcomings (TODO: still needs to be done)
-- * Treatment of Booleans:
-- * * type printed as bool, should be int instead
-- * * constants would have to be defined as int True = 1; and int False = 0;
-- * Variable declarations and sourrounding code still (fct. main etc.) still has to be printed

-- Imperative programming language
data Imp l
  = Assign Var Expr
  | Goto l
  | IfThen Expr (Imp l)
  | Labeled l (Imp l)
  | Seq [Imp l]

cmdToImp :: Cmd -> Imp l
cmdToImp (VAssign vr e) = Assign vr e

transToImp :: Transition l -> Imp l
transToImp tr =
  IfThen (guardOfTransition tr)
         (Seq (map cmdToImp (actionOfTransition tr) ++ [Goto (targetOfTransition tr)]))

locTransToImp :: (l, [Transition l]) -> Imp l
locTransToImp (g, trs) = Labeled g (Seq (map transToImp trs))

autToImp :: Eq l => Aut l -> Imp l
autToImp a =
  let partitionedTrans = map (\l -> (l, [tr | tr <- transitionsOfAut a, sourceOfTransition tr == l])) (locsOfAut a)
  in Seq (Goto (initialLocOfAut a) : map locTransToImp partitionedTrans)


class ShowLabel l where
  showLabel :: l -> String
  showLabelList :: [l] -> String

instance ShowLabel Char where
  showLabel = show
  showLabelList s = s

instance ShowLabel l => ShowLabel [l] where
  showLabel = showLabelList
  showLabelList ss = foldr (<>)  "" (Data.List.intersperse "_" (map showLabel ss))

class ShowImp x where
    showImp :: x -> Doc ann

instance ShowImp Lit where
  showImp (IntLit i) = pretty (show i)
  showImp (BoolLit b) = pretty (show b)

instance ShowImp Builtin where
  showImp Minus = pretty "-"
  showImp Sum = pretty "+"
  showImp Product = pretty "+"
  showImp Not = pretty "!"
  showImp Lt = pretty "<"
  showImp Le = pretty "<="
  showImp Gt = pretty ">"
  showImp Ge = pretty ">="
  showImp Eq = pretty "=="
  showImp Ne = pretty "!="
  showImp And = pretty "&&"
  showImp Or = pretty "||"

instance ShowImp Expr where
  showImp (Var v) = pretty v
  showImp (Lit l) = showImp l
  showImp (Builtin Not [e]) = showImp Not <+> showImp e   -- assuming this is a prefix unary
  showImp (Builtin b [e1, e2]) = showImp e1 <+> showImp b <+> showImp e2
  showImp (Builtin b es) = showImp (normalizeExpr b es)

normalizeExpr :: Builtin -> [Expr] -> Expr
normalizeExpr Sum [] = il 0
normalizeExpr Product [] = il 1
normalizeExpr And [] = bl True
normalizeExpr Or [] = bl False
normalizeExpr _ [e] = e
normalizeExpr b (e:es) = Builtin b [e, normalizeExpr b es]
normalizeExpr b es = Builtin b es  -- error case (malformed expression)

nestingDepth :: Int
nestingDepth = 4

instance ShowLabel l => ShowImp (Imp l) where
  showImp (Assign v e) = pretty v <+> pretty "=" <+> showImp e <+> pretty ";"
  showImp (Goto l) = pretty "goto" <+> pretty (showLabel l) <+> pretty ";"
  showImp (IfThen e imp) = pretty "if" <+> parens (showImp e) <+> braces (nest nestingDepth (line <> align (showImp imp)))
  showImp (Labeled l imp) = pretty (showLabel l) <+> pretty ":" <+> line <> showImp imp <+> pretty ";"
  showImp (Seq imps) = vsep (map showImp imps)

printAut :: (ShowLabel l, Eq l) => Aut l -> IO()
printAut a = print (showImp (autToImp a))

----------------------------------------------------------------------------------------
-- Some tests
----------------------------------------------------------------------------------------

transA :: [Transition String]
transA = [ Transition "l1A" (bl True) (SendSync "c1") [VAssign "x" (il 3)] "l2A"
         , Transition "l1A" (bl True) NoSync [VAssign "x" (il 4)] "l3A"
         ]
autA :: Aut String
autA = Aut "autA" ["l1A", "l2A", "l3A"] transA "l1A"

transAm :: [Transition String]
transAm = [ Transition "l1A" (bl True) (SendSync "c1") [] "l2A"
         ,  Transition "l1A" (bl True) (SendSync "c2") [] "l3A"
         ]
autAm :: Aut String
autAm = Aut "autA" ["l1A", "l2A", "l3A"] transAm "l1A"

transB :: [Transition String]
transB = [ Transition "l1B" (bl True) (RecvSync "c1") [] "l2B"
         ]
autB :: Aut String
autB = Aut "autB" ["l1B", "l2B"] transB "l1B"

transC :: [Transition String]
transC = [ Transition "l1C" (bl True) NoSync [VAssign "x" (il 5)] "l2C"
         ]

autC :: Aut String
autC = Aut "autC" ["l1C", "l2C"] transC "l1C"

sysAB :: Sys String
sysAB = Sys ["x"] ["c1", "c2"] [autA, autB]

sysABm :: Sys String
sysABm = Sys ["x"] ["c1", "c2"] [autAm, autB]

sysABC :: Sys String
sysABC = Sys ["x"] ["c1", "c2"] [autA, autB, autC]

-- >>> autToDot nameProdAut edgeLabelAut currAut
-- "compactPropGraph.pdf"

-- >>> minAut (prodAut sysABm)
-- Aut {nameOfAut = "prod", locsOfAut = [["l1A","l1B"],["l2A","l2B"],["l3A","l1B"],["l3A","l2B"]], transitionsOfAut = [Transition {sourceOfTransition = ["l1A","l1B"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Just "c1", actionOfTransition = [], targetOfTransition = ["l2A","l2B"]},Transition {sourceOfTransition = ["l1A","l1B"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Just "c2", actionOfTransition = [], targetOfTransition = ["l3A","l1B"]},Transition {sourceOfTransition = ["l3A","l1B"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = Just "c1", actionOfTransition = [], targetOfTransition = ["l3A","l2B"]}], initialLocOfAut = ["l1A","l1B"]}

----------------------------------------------------------------------------------------
-- Company purchase case study
----------------------------------------------------------------------------------------

companyAut :: Aut String
companyAut = Aut {
    nameOfAut = "Company"
  , locsOfAut = ["independent", "acquisitionInCourse", "sold"]
  , transitionsOfAut = [
      Transition "independent" (bl True) (RecvSync "startAcquisition") [] "acquisitionInCourse"
    , Transition "acquisitionInCourse" (bl True) (RecvSync "payment") [] "acquisitionInCourse"
    , Transition "acquisitionInCourse" (bl True) (RecvSync "endAcquisition") [] "sold"
  ]
  , initialLocOfAut = "independent"
}

masAut :: Aut String
masAut = Aut {
    nameOfAut = "MAS"
  , locsOfAut = ["masInit", "masConsulted"]
  , transitionsOfAut = [
      Transition "masInit" (bl True) (RecvSync "requestPermission") [] "masConsulted"
    , Transition "masConsulted" (bl True) (SendSync "grantPermission") [] "masInit"
    , Transition "masConsulted" (bl True) (SendSync "refusePermission") [] "masInit"
  ]
  , initialLocOfAut = "masInit"
}

trPStartAcquisition :: Transition String
trPStartAcquisition = Transition{
    sourceOfTransition = "pInit"
  , guardOfTransition = bl True
  , syncOfTransition = SendSync "startAcquisition"
  , actionOfTransition = [
        VAssign "controlled" (bl False)
      , VAssign "paymentComplete"  (bl False)
      , VAssign "debt" (il 100)
      , VAssign "installment" (il 40)
      , VAssign "interest" (il 0)
      ]
  , targetOfTransition = "pLoopHead"
  }

purchaserAut :: Aut String
purchaserAut = Aut {
    nameOfAut = "Purchaser"
  , locsOfAut = ["pInit", "pLoopHead", "pPermissionRequested", "pPermissionRefused",
                 "pPermissionGranted", "pCheckDebt", "pPaymentDone", "pFinal"
                ]
  , transitionsOfAut = [
      trPStartAcquisition
    , Transition "pLoopHead" (bl True) (SendSync "requestPermission") [] "pPermissionRequested"
    , Transition "pPermissionRequested" (bl True) (RecvSync "refusePermission") [] "pPermissionRefused"
    , Transition "pPermissionRequested" (bl True) (RecvSync "grantPermission") []  "pPermissionGranted"
    , Transition "pPermissionGranted" (bl True) (SendSync "payment") [VAssign "debt" (Var "debt" `minus` Var "installment") ] "pCheckDebt"
    , Transition "pCheckDebt" (Var "debt" `le` il 0) NoSync [VAssign "paymentComplete" (bl True), VAssign "controlled" (bl True)] "pPaymentDone"
    , Transition "pCheckDebt" (Var "debt" `gt` il 0) NoSync [VAssign "debt" (Var "debt" `plus` Var "interest")] "pLoopHead"
    , Transition "pPaymentDone" (bl True) (SendSync "endAcquisition") [] "pFinal"
  ]
  , initialLocOfAut = "pInit"
}

-- >>> autToDot id edgeLabelAut purchaserAut
-- "compactPropGraph.pdf"

sysCompanyPurchase :: Sys String
sysCompanyPurchase = Sys {
    declsOfSys = ["controlled", "paymentComplete", "debt", "installment", "interest"]
  , channelsOfSys = ["requestPermission", "grantPermission", "refusePermission",
    "startAcquisition", "endAcquisition", "payment"]
  , automataOfSys = [companyAut, masAut, purchaserAut]
  }

minSysCompanyPurchase :: Aut [String]
minSysCompanyPurchase = minAut (prodAut sysCompanyPurchase)

-- >>> length (locsOfAut minSysCompanyPurchase)
-- 8

-- >>> autToDot nameProdAut edgeLabelAut minSysCompanyPurchase
-- "compactPropGraph.pdf"

-- >>> runUpctlSearch minSysCompanyPurchase 20 (upform E F (variableHasBoolValS "paymentComplete"))
-- UpCTLSearchState {upctlResult = Valid, upctlTrace = (State {currLoc = ["acquisitionInCourse","masInit","pPaymentDone"], assignments = fromList [("controlled",BoolVal True),("debt",IntVal (-20)),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal True)]},[(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Builtin Le [Var "debt",Lit (IntLit 0)]], syncOfTransition = NoSync, actionOfTransition = [VAssign "paymentComplete" (Lit (BoolLit True)),VAssign "controlled" (Lit (BoolLit True))], targetOfTransition = ["acquisitionInCourse","masInit","pPaymentDone"]},State {currLoc = ["acquisitionInCourse","masInit","pCheckDebt"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal (-20)),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "payment", actionOfTransition = [VAssign "debt" (Builtin Minus [Var "debt",Var "installment"])], targetOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"]},State {currLoc = ["acquisitionInCourse","masInit","pPermissionGranted"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "grantPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Builtin Gt [Var "debt",Lit (IntLit 0)]], syncOfTransition = NoSync, actionOfTransition = [VAssign "debt" (Builtin Sum [Var "debt",Var "interest"])], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["acquisitionInCourse","masInit","pCheckDebt"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "payment", actionOfTransition = [VAssign "debt" (Builtin Minus [Var "debt",Var "installment"])], targetOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"]},State {currLoc = ["acquisitionInCourse","masInit","pPermissionGranted"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "grantPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Builtin Gt [Var "debt",Lit (IntLit 0)]], syncOfTransition = NoSync, actionOfTransition = [VAssign "debt" (Builtin Sum [Var "debt",Var "interest"])], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["acquisitionInCourse","masInit","pCheckDebt"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "payment", actionOfTransition = [VAssign "debt" (Builtin Minus [Var "debt",Var "installment"])], targetOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"]},State {currLoc = ["acquisitionInCourse","masInit","pPermissionGranted"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "grantPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["independent","masInit","pInit"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "startAcquisition", actionOfTransition = [VAssign "controlled" (Lit (BoolLit False)),VAssign "paymentComplete" (Lit (BoolLit False)),VAssign "debt" (Lit (IntLit 100)),VAssign "installment" (Lit (IntLit 40)),VAssign "interest" (Lit (IntLit 0))], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["independent","masInit","pInit"], assignments = fromList []})])}

-- >>> runUpctlSearch minSysCompanyPurchase 20 (upform A F (variableHasBoolValS "paymentComplete"))
-- UpCTLSearchState {upctlResult = Invalid, upctlTrace = (State {currLoc = ["acquisitionInCourse","masInit","pPermissionRefused"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]},[(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "refusePermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionRefused"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Builtin Gt [Var "debt",Lit (IntLit 0)]], syncOfTransition = NoSync, actionOfTransition = [VAssign "debt" (Builtin Sum [Var "debt",Var "interest"])], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["acquisitionInCourse","masInit","pCheckDebt"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 20),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "payment", actionOfTransition = [VAssign "debt" (Builtin Minus [Var "debt",Var "installment"])], targetOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"]},State {currLoc = ["acquisitionInCourse","masInit","pPermissionGranted"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "grantPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Builtin Gt [Var "debt",Lit (IntLit 0)]], syncOfTransition = NoSync, actionOfTransition = [VAssign "debt" (Builtin Sum [Var "debt",Var "interest"])], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["acquisitionInCourse","masInit","pCheckDebt"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 60),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "payment", actionOfTransition = [VAssign "debt" (Builtin Minus [Var "debt",Var "installment"])], targetOfTransition = ["acquisitionInCourse","masInit","pCheckDebt"]},State {currLoc = ["acquisitionInCourse","masInit","pPermissionGranted"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "grantPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masInit","pPermissionGranted"]},State {currLoc = ["acquisitionInCourse","masConsulted","pPermissionRequested"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["acquisitionInCourse","masInit","pLoopHead"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "requestPermission", actionOfTransition = [], targetOfTransition = ["acquisitionInCourse","masConsulted","pPermissionRequested"]},State {currLoc = ["acquisitionInCourse","masInit","pLoopHead"], assignments = fromList [("controlled",BoolVal False),("debt",IntVal 100),("installment",IntVal 40),("interest",IntVal 0),("paymentComplete",BoolVal False)]}),(Transition {sourceOfTransition = ["independent","masInit","pInit"], guardOfTransition = Builtin And [Lit (BoolLit True),Lit (BoolLit True),Lit (BoolLit True)], syncOfTransition = SendSync "startAcquisition", actionOfTransition = [VAssign "controlled" (Lit (BoolLit False)),VAssign "paymentComplete" (Lit (BoolLit False)),VAssign "debt" (Lit (IntLit 100)),VAssign "installment" (Lit (IntLit 40)),VAssign "interest" (Lit (IntLit 0))], targetOfTransition = ["acquisitionInCourse","masInit","pLoopHead"]},State {currLoc = ["independent","masInit","pInit"], assignments = fromList []})])}


currAut :: Aut [String]
currAut = minSysCompanyPurchase
