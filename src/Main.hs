
import Language.Haskell.Migrate.MRP
import Options.Applicative
import Control.Logging

-- ---------------------------------------------------------------------

migrate :: Opts -> IO [FilePath]
migrate o = do
  withStdoutLogging $ do
    if debugOn o
      then setLogLevel LevelDebug
      else setLogLevel LevelError
    mrp (file o)
{-
withStdoutLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStderrLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withFileLogging :: (MonadBaseControl IO m, MonadIO m) => FilePath -> m a -> m a
-}

-- ---------------------------------------------------------------------

data Opts = Opts
  { file    :: FilePath
  , debugOn :: Bool
  }

optsp :: Parser Opts
optsp = Opts
     <$> argument str (metavar "FILE")
     <*> switch
         ( long "debug"
        <> short 'd'
        <> help "generate debug output" )

-- ---------------------------------------------------------------------

main :: IO [FilePath]
main = execParser opts >>= migrate
  where
    opts = info (helper <*> optsp)
      ( fullDesc
     <> progDesc "migrate FILE according to MRP"
     <> header "migrate - migrate source according to MRP" )
