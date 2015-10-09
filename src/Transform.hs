{-# LANGUAGE StandaloneDeriving #-}
module Transform (mrp) where

import qualified Data.Generics         as SYB

import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified RdrName       as GHC

import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix

import qualified Data.Map as Map
-- import Debug.Trace
-- import Control.Exception

-- ---------------------------------------------------------------------

mrp :: FilePath ->IO [FilePath]
mrp fileName = do
  absFileName <- canonicalizePath fileName
  comp fileName

comp :: FilePath -> IO [FilePath]
comp fileName = do
  pr <- parseModule fileName
  case pr of
    Left (ss,errStr) -> error $ "parse failed for " ++ fileName ++ ":" ++ errStr
    Right (anns,parsed) -> do
      let insts = getOneParamInstances parsed
      putStrLn $ "Transform.com:instances:" ++ showGhc insts
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
            putStrLn $ "Found matching applicative:" ++ showGhc a
            -- moveReturn i a
            let (lp',(ans',_),_w) = runTransform anns (moveReturn parsed i a)
            putStrLn $ "log:\n" ++ intercalate "\n" _w
            writeResults lp' ans' fileName
            return ()
        return []

      return [fileName]

-- ---------------------------------------------------------------------

writeResults :: GHC.ParsedSource -> Anns -> FilePath -> IO ()
writeResults parsed ann fileName = do
  let source = exactPrint parsed ann
  let (baseFileName,ext) = splitExtension fileName
  writeFile (baseFileName ++ ".refactored" ++ ext) source

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
moveReturn :: GHC.ParsedSource
           -> GHC.ClsInstDecl GHC.RdrName -> GHC.ClsInstDecl GHC.RdrName
           -> Transform GHC.ParsedSource
moveReturn parsed monad applicative = do
  logTr $ "moveReturn:moving (from,to):" ++ showGhc (monad,applicative)
  let returnFb = fromJust $ findFunBind returnRdrName monad
      pureFb   = fromJust $ findFunBind pureRdrName   applicative
  logTr $ "moveReturn:moving (returnFb,pureFb):" ++ showGhc (returnFb,pureFb)
  let parsed2 = replaceMatchGroup parsed pureFb (GHC.fun_matches (GHC.unLoc returnFb))
  grhs <- liftT grhsPure
  return $ replaceGrHs parsed2 returnFb grhs

-- ---------------------------------------------------------------------

findFunBind :: (SYB.Data t) => GHC.RdrName -> t -> Maybe (GHC.LHsBind GHC.RdrName)
findFunBind rdr t = SYB.something (Nothing `SYB.mkQ` bind) t
  where
    bind :: GHC.LHsBind GHC.RdrName -> Maybe (GHC.LHsBind GHC.RdrName)
    bind b@(GHC.L _ fb@(GHC.FunBind{}))
      | (GHC.unLoc $ GHC.fun_id fb) == rdr = Just b
    bind _ = Nothing

-- ---------------------------------------------------------------------

replaceMatchGroup :: GHC.ParsedSource
                  -> GHC.LHsBind GHC.RdrName
                  -> GHC.MatchGroup GHC.RdrName (GHC.LHsExpr GHC.RdrName)
                  -> GHC.ParsedSource
replaceMatchGroup parsed (GHC.L l fb) mg = do
  SYB.everywhere (SYB.mkT doReplace) parsed
  where
    doReplace (GHC.L lf _fb1)
      | lf == l = (GHC.L lf (fb { GHC.fun_matches = mg}))
    doReplace x = x

-- ---------------------------------------------------------------------

replaceGrHs :: GHC.ParsedSource
            -> GHC.LHsBind GHC.RdrName
            -> (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName))
            -> GHC.ParsedSource
replaceGrHs parsed (GHC.L l _fb) grhs = do
  SYB.everywhere (SYB.mkT doReplace) parsed
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

-- ---------------------------------------------------------------------

-- |Make a simple unqualified 'GHC.RdrName'
mkRdrName :: String -> GHC.RdrName
mkRdrName s = GHC.mkVarUnqual (GHC.mkFastString s)

-- ---------------------------------------------------------------------
