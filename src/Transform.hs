{-# LANGUAGE StandaloneDeriving #-}
module Transform (mrp) where

import qualified Data.Generics         as SYB
-- import qualified GHC.SYB.Utils         as SYB

-- import qualified BasicTypes    as GHC
import qualified GHC           as GHC

import qualified Language.Haskell.GhcMod as GM (Options(..))
import Language.Haskell.Refact.API

-- To be moved into HaRe API
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
-- import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad
import Data.Maybe
import System.Directory
import qualified Data.Map as Map
-- import Debug.Trace
-- import Control.Exception

-- ---------------------------------------------------------------------

mrp :: RefactSettings -> GM.Options -> FilePath ->IO [FilePath]
mrp settings opts fileName = do
  absFileName <- canonicalizePath fileName
  runRefacSession settings opts (doRefact absFileName)

doRefact :: FilePath -> RefactGhc [ApplyRefacResult]
doRefact absFileName = do
  (r,_) <- applyRefac (comp absFileName ) (RSFile absFileName)
  return [r]

comp :: FilePath -> RefactGhc ()
comp fileName = do
       parseSourceFileGhc fileName
       parsed <- getRefactParsed
       logDataWithAnns "Transform.comp:parsed="  parsed -- ++AZ++
       let insts = getOneParamInstances parsed
       logm $ "Transform.com:instances:" ++ showGhc insts
       let
         isInstance name ((s,_),_) = showGhc s == name
         byParamMap is = Map.fromList $ map (\v@((_,k),_) -> (showGhc k,v)) is
         applicatives = byParamMap $ filter (isInstance "Applicative") insts
         monads       = filter (isInstance "Monad") insts
       -- logm $ "Transform.com:Applicative instances:" ++ showGhc applicatives
       -- logm $ "Transform.com:Monad instances:" ++ showGhc monads

       void $ forM monads $ \((s,p),i) -> do
         case Map.lookup (showGhc p) applicatives of
           Nothing -> do
             error $ "Need to create an Applicative instance for:" ++ showGhc (s, p)
           Just (_,a) -> do
             logm $ "Found matching applicative:" ++ showGhc a
             moveReturn i a
         return ()

       return ()

-- ---------------------------------------------------------------------

getOneParamInstances :: GHC.ParsedSource -> [((GHC.RdrName, GHC.LHsType GHC.RdrName),GHC.ClsInstDecl GHC.RdrName)]
getOneParamInstances parsed =
  SYB.everything (++) ([] `SYB.mkQ` cls) parsed
  where
    cls c@(GHC.ClsInstDecl typ _binds _sigs _tfis _dfis _mo)
      = case getInstanceTypes typ of
          Nothing -> []
          Just ts -> [(ts,c)]

    getInstanceTypes (GHC.L _ (GHC.HsForAllTy _ _ _ _ (GHC.L _ (GHC.HsAppTy (GHC.L _ (GHC.HsTyVar v1)) v2))))
      = Just (v1,v2)
    getInstanceTypes _ = Nothing

-- ---------------------------------------------------------------------

returnRdrName :: GHC.RdrName
returnRdrName = mkRdrName "return"

pureRdrName :: GHC.RdrName
pureRdrName = mkRdrName "pure"

-- ---------------------------------------------------------------------

-- |Move the implementation of the 'return' function from the monad instance to
-- the 'pure' function in the applicative
moveReturn :: GHC.ClsInstDecl GHC.RdrName -> GHC.ClsInstDecl GHC.RdrName -> RefactGhc ()
moveReturn monad applicative = do
  logm $ "moveReturn:moving (from,to):" ++ showGhc (monad,applicative)
  let returnFb = fromJust $ findFunBind returnRdrName monad
      pureFb   = fromJust $ findFunBind pureRdrName   applicative
  logm $ "moveReturn:moving (returnFb,pureFb):" ++ showGhc (returnFb,pureFb)
  replaceMatchGroup pureFb (GHC.fun_matches (GHC.unLoc returnFb))
  grhs <- liftT grhsPure
  replaceGrHs returnFb grhs
  return ()

-- ---------------------------------------------------------------------

findFunBind :: (SYB.Data t) => GHC.RdrName -> t -> Maybe (GHC.LHsBind GHC.RdrName)
findFunBind rdr t = SYB.something (Nothing `SYB.mkQ` bind) t
  where
    bind :: GHC.LHsBind GHC.RdrName -> Maybe (GHC.LHsBind GHC.RdrName)
    bind b@(GHC.L _ fb@(GHC.FunBind{}))
      | (GHC.unLoc $ GHC.fun_id fb) == rdr = Just b
    bind _ = Nothing

-- ---------------------------------------------------------------------

replaceMatchGroup :: GHC.LHsBind GHC.RdrName
                  -> GHC.MatchGroup GHC.RdrName (GHC.LHsExpr GHC.RdrName)
                  -> RefactGhc ()
replaceMatchGroup (GHC.L l fb) mg = do
  parsed <- getRefactParsed
  let parsed' = SYB.everywhere (SYB.mkT doReplace) parsed
  putRefactParsed parsed' emptyAnns
  where
    doReplace (GHC.L lf _fb1)
      | lf == l = (GHC.L lf (fb { GHC.fun_matches = mg}))
    doReplace x = x

-- ---------------------------------------------------------------------

replaceGrHs :: GHC.LHsBind GHC.RdrName
            -> (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName))
            -> RefactGhc ()
replaceGrHs (GHC.L l _fb) grhs = do
  parsed <- getRefactParsed
  let parsed' = SYB.everywhere (SYB.mkT doReplace) parsed
  putRefactParsed parsed' emptyAnns
  where
    doReplace :: GHC.LHsBind GHC.RdrName -> GHC.LHsBind GHC.RdrName
    doReplace (GHC.L lf fb1)
      | lf == l = (GHC.L lf (fb1 { GHC.fun_matches = mg'}))
      where
        (GHC.MG (lmatch:_) tys tym o) = GHC.fun_matches fb1
        (GHC.L lm (GHC.Match mid pats ty _grhss)) = lmatch
        match' = (GHC.L lm (GHC.Match mid pats ty grhs))
        mg' = (GHC.MG [match'] tys tym o)
    doReplace x = x

-- ---------------------------------------------------------------------

grhsPure :: Transform (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName))
grhsPure = do
  grhsLoc <- uniqueSrcSpanT
  pureLoc <- uniqueSrcSpanT
  let
    lPure = (GHC.L pureLoc (GHC.HsVar pureRdrName))
    lGrhs = (GHC.L grhsLoc (GHC.GRHS [] lPure)) :: GHC.LGRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName)
    ret   = (GHC.GRHSs [lGrhs] GHC.EmptyLocalBinds)
  addSimpleAnnT lGrhs (DP (0,0)) []
  addSimpleAnnT lPure (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  return ret
{-
-- MatchGroup required for "return = pure"

  (MG
   [
    ({ testcases/H2010/Maybe1.hs:24:5-17 }
     Just (Ann (DP (0,0)) [] [] [((G AnnEqual),DP (0,1))] Nothing Nothing)
     (Match
      (Just
       ((,)
        ({ testcases/H2010/Maybe1.hs:24:5-10 }
         Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
         (Unqual {OccName: return}))
        (False)))
      []
      (Nothing)
      (GRHSs
       [
        ({ testcases/H2010/Maybe1.hs:24:12-17 }
         Just (Ann (DP (0,-1)) [] [] [] Nothing Nothing)
         (GRHS
          []
          ({ testcases/H2010/Maybe1.hs:24:14-17 }
           Just (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
           (HsVar
            (Unqual {OccName: pure})))))]
       (EmptyLocalBinds))))]
   []
   (PlaceHolder)
   (FromSource))
-}
