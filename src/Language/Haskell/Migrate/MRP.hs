{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Migrate.MRP (mrp) where

import qualified GHC.SYB.Utils as SYB
import qualified Data.Generics         as SYB

import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Control.Logging as L
import qualified Data.Text as T
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
      -- putStrLn $ "parsed=" ++ SYB.showData SYB.Parser 0 parsed
      (appAnns,applicativeTemplate) <- applicativeInstanceTemplate
      (funAnns,functorTemplate)     <- functorInstanceTemplate
      let anns' = mergeAnnList [anns, appAnns, funAnns]
      let (lp',(ans',_),_w) = runTransform anns' (process parsed applicativeTemplate functorTemplate)
      L.debug $ T.pack $ "log:\n" ++ intercalate "\n" _w
      writeResults lp' ans' fileName
      return [fileName]

-- ---------------------------------------------------------------------

process :: GHC.ParsedSource -> GHC.LHsDecl GHC.RdrName -> GHC.LHsDecl GHC.RdrName
        -> Transform GHC.ParsedSource
process parsed appTemplate funTemplate = do
  foldM processOne parsed monads
  where
    insts = getOneParamInstances parsed
    isInstance name ((s,_),_) = showGhc s == name
    byParamMap is = Map.fromList $ map (\v@((_,k),_) -> (showGhc k,v)) is
    applicatives = byParamMap $ filter (isInstance "Applicative") insts
    functors     = byParamMap $ filter (isInstance "Functor")     insts
    monads       = filter (isInstance "Monad") insts

    processOne :: GHC.ParsedSource -> ((GHC.RdrName, GHC.LHsType GHC.RdrName), GHC.ClsInstDecl GHC.RdrName)
               -> Transform GHC.ParsedSource
    processOne parsed ((s,p),i) = do
      (p2,a) <- addApplicativeIfNeeded parsed appTemplate applicatives ((s,p),i)
      p3 <- addFunctorIfNeeded p2 funTemplate functors ((s,p),i)
      -- logDataWithAnnsTr "after adding applicative:" p3
      p4 <- moveReturn p3 i a
      return p4

-- ---------------------------------------------------------------------

addFunctorIfNeeded :: GHC.ParsedSource
                   -> GHC.LHsDecl GHC.RdrName
                   -> Map.Map String a
                   -> ((GHC.RdrName,GHC.LHsType GHC.RdrName),d)
                   -> Transform GHC.ParsedSource
addFunctorIfNeeded parsed funTemplate functors ((s,p),i) = do
  case Map.lookup (showGhc p) functors of
    Nothing -> do
      funTemplate' <- replaceRdrName placeholderRdrName p funTemplate
      logDataWithAnnsTr "funTemplate'" funTemplate'
      logDataWithAnnsTr "p" p
      decls <- hsDecls parsed
      parsed' <- replaceDecls parsed (funTemplate':decls)
      logTr $ "addFunctorIfNeeded:added"
      return parsed'
    Just _ -> return parsed

-- ---------------------------------------------------------------------

addApplicativeIfNeeded :: GHC.ParsedSource
                       -> GHC.LHsDecl GHC.RdrName
                       -> Map.Map String ((GHC.RdrName, GHC.LHsType GHC.RdrName), GHC.ClsInstDecl GHC.RdrName)
                       -> ((GHC.RdrName,GHC.LHsType GHC.RdrName),d)
                       -> Transform (GHC.ParsedSource,GHC.ClsInstDecl GHC.RdrName)
addApplicativeIfNeeded parsed appTemplate applicatives ((s,p),i) = do
  case Map.lookup (showGhc p) applicatives of
    Nothing -> do
      appTemplate' <- replaceRdrName placeholderRdrName p appTemplate
      -- logDataWithAnnsTr "appTemplate'" appTemplate'
      logTr $ "appTemplate'=" ++ showGhc appTemplate'
      decls <- hsDecls parsed
      parsed' <- replaceDecls parsed (appTemplate':decls)
      let (GHC.L _ (GHC.InstD (GHC.ClsInstD instDecl))) = appTemplate
      logTr $ "addApplicativeIfNeeded:added"
      return (parsed', instDecl )
    Just (_,a) -> return (parsed,a)

-- ---------------------------------------------------------------------

applicativeInstanceTemplate :: IO (Anns,GHC.LHsDecl GHC.RdrName)
applicativeInstanceTemplate = parseDeclAnns "ap" applicativeStr
  where
    applicativeStr :: String
    applicativeStr =
      "instance Applicative PLACEHOLDER where\n\
      \  pure = undefined\n\
      \  f1 <*> f2   = f1 >>= \\v1 -> f2 >>= (pure . v1)\n"

-- ---------------------------------------------------------------------

functorInstanceTemplate :: IO (Anns,GHC.LHsDecl GHC.RdrName)
functorInstanceTemplate = parseDeclAnns "fn" functorStr
  where
    functorStr :: String
    functorStr =
      "instance Functor PLACEHOLDER where\n\
      \  fmap f m    = m >>= pure . f\n"

-- ---------------------------------------------------------------------

parseDeclAnns tag str = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df  tag str)
  let declAnns' = setPrecedingLines newDecl 2 0 declAnns
  return (declAnns',newDecl)

-- ---------------------------------------------------------------------

writeResults :: GHC.ParsedSource -> Anns -> FilePath -> IO ()
writeResults parsed ann fileName = do
  let source = exactPrint parsed ann
  let (baseFileName,ext) = splitExtension fileName
  writeFile (baseFileName ++ ".refactored" ++ ext) source
  writeFile (fileName ++ ".AST_out") (showAnnData ann 0 parsed)

-- ---------------------------------------------------------------------

getOneParamInstances :: GHC.ParsedSource
                     -> [((GHC.RdrName, GHC.LHsType GHC.RdrName),GHC.ClsInstDecl GHC.RdrName)]
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

placeholderRdrName :: GHC.RdrName
placeholderRdrName = mkRdrName "PLACEHOLDER"

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

replaceRdrName :: (SYB.Typeable t,SYB.Data t)
               => GHC.RdrName -> GHC.LHsType GHC.RdrName -> t -> Transform t
replaceRdrName old new t = SYB.everywhereM (SYB.mkM doReplaceTyVar) t
  where
    doReplaceTyVar :: GHC.LHsType GHC.RdrName -> Transform (GHC.LHsType GHC.RdrName)
    doReplaceTyVar (GHC.L l (GHC.HsTyVar rn))
      | showGhc rn == showGhc old = do
          -- logTr $ "doReplaceTyVar: returning " ++ showGhc (GHC.L l new)
          return new
    doReplaceTyVar x = return x

-- ---------------------------------------------------------------------

replaceMatchGroup :: GHC.ParsedSource
                  -> GHC.LHsBind GHC.RdrName
                  -> GHC.MatchGroup GHC.RdrName (GHC.LHsExpr GHC.RdrName)
                  -> GHC.ParsedSource
replaceMatchGroup parsed (GHC.L l fb) mg = do
  SYB.everywhere (SYB.mkT doReplace) parsed
  where
    doReplace (GHC.L lf _fb1)
      | lf == l = (GHC.L lf (fb { GHC.fun_matches = replaceReturnWithPure mg}))
    doReplace x = x

-- ---------------------------------------------------------------------

replaceReturnWithPure :: (SYB.Typeable t,SYB.Data t) => t -> t
replaceReturnWithPure t = SYB.everywhere (SYB.mkT doReplace) t
  where
    doReplace :: GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName)
              -> GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName)
    doReplace (GHC.Match (Just (GHC.L l rn,ii)) pats typ rhs)
      | rn == returnRdrName = (GHC.Match (Just (GHC.L l pureRdrName,ii)) pats typ rhs)
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

-- |The annotations are keyed to the constructor, so if we replace a qualified
-- with an unqualified RdrName or vice versa we have to rebuild the key for the
-- appropriate annotation.
replaceAnnKey :: (SYB.Data old,SYB.Data new)
  => GHC.Located old -> GHC.Located new -> Anns -> Anns
replaceAnnKey old new ans =
  case Map.lookup (mkAnnKey old) ans of
    Nothing -> ans
    Just v ->  anns'
      where
        anns1 = Map.delete (mkAnnKey old) ans
        anns' = Map.insert (mkAnnKey new) v anns1
{-

    doReplaceRdr :: GHC.Located GHC.RdrName -> Transform (GHC.Located GHC.RdrName)
    doReplaceRdr oldName@(GHC.L l rn)
      | rn == old = do
        let newName = (GHC.L l new)
        modifyAnnsT (replaceAnnKey oldName newName)
        return newName
    doReplaceRdr x = return x
-}
-- ---------------------------------------------------------------------

copyAnn :: (SYB.Data old,SYB.Data new)
  => GHC.Located old -> GHC.Located new -> Anns -> Anns
copyAnn old new ans =
  case Map.lookup (mkAnnKey old) ans of
    Nothing -> ans
    Just v ->  anns'
      where
        anns' = Map.insert (mkAnnKey new) v ans

-- ---------------------------------------------------------------------
