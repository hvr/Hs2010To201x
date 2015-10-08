{-# LANGUAGE StandaloneDeriving #-}
module Transform (mrp) where

import qualified Data.Generics         as SYB
import qualified GHC.SYB.Utils         as SYB

import qualified BasicTypes    as GHC
import qualified GHC           as GHC

import qualified Language.Haskell.GhcMod as GM (Options(..))
import Language.Haskell.Refact.API

-- To be moved into HaRe API
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import System.Directory
import qualified Data.Map as Map
-- import Debug.Trace
import Control.Exception

-- ---------------------------------------------------------------------

mrp :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> SimpPos -> IO [FilePath]
mrp settings opts fileName beginPos endPos = do
  absFileName <- canonicalizePath fileName
  runRefacSession settings opts (comp absFileName beginPos endPos)

comp :: FilePath -> SimpPos -> SimpPos -> RefactGhc [ApplyRefacResult]
comp fileName beginPos endPos = do
       parseSourceFileGhc fileName
       parsed <- getRefactParsed
       logDataWithAnns "Transform.comp:parsed="  parsed -- ++AZ++
       let insts = getOneParamInstances parsed
       logm $ "Transform.com:instances:" ++ showGhc insts
       let
         isApplicativeOrMonad ((s,_),_) = showGhc s == "Applicative" || showGhc s == "Monad"
         amis = filter isApplicativeOrMonad insts
       logm $ "Transform.com:Applicative/Monad instances:" ++ showGhc amis
       -- let expr = locToExp beginPos endPos parsed
       -- case expr of
       --   Just exp1@(GHC.L _ (GHC.HsIf _ _ _ _))
       --          -> do (refactoredMod,_) <- applyRefac (doIfToCaseInternal exp1) RSAlreadyLoaded
       --                return [refactoredMod]
       --   _      -> error $ "You haven't selected an if-then-else  expression!" -- (show (beginPos,endPos,fileName)) ++ "]:" ++ (SYB.showData SYB.Parser 0 $ ast)
       assert False undefined

-- ---------------------------------------------------------------------

getOneParamInstances :: GHC.ParsedSource -> [((GHC.RdrName, GHC.LHsType GHC.RdrName),GHC.ClsInstDecl GHC.RdrName)]
getOneParamInstances parsed =
  SYB.everything (++) ([] `SYB.mkQ` cls) parsed
  where
    cls c@(GHC.ClsInstDecl typ binds sigs tfis dfis mo)
      = case getInstanceTypes typ of
          Nothing -> []
          Just ts -> [(ts,c)]

    getInstanceTypes (GHC.L _ (GHC.HsForAllTy _ _ _ _ (GHC.L _ (GHC.HsAppTy (GHC.L _ (GHC.HsTyVar v1)) v2))))
      = Just (v1,v2)
    getInstanceTypes _ = Nothing

    isMonadTy (GHC.L _ (GHC.HsForAllTy _ _ _ _ (GHC.L _ (GHC.HsAppTy (GHC.L _ (GHC.HsTyVar v1)) v2))))
      = showGhc v1 == "Monad"
    isMonadTy _ = False
