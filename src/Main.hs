
import Language.Haskell.Migrate.MRP
import Options.Applicative
import Control.Logging

-- ---------------------------------------------------------------------

migrate :: Opts -> IO ()
migrate o = do
  withStdoutLogging $ do
    if debugOn o
      then setLogLevel LevelDebug
      else setLogLevel LevelError
    -- putStrLn $ show o
    return []
    mapM_ (mrp o) (files o)
{-
withStdoutLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStderrLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withFileLogging :: (MonadBaseControl IO m, MonadIO m) => FilePath -> m a -> m a
-}

-- ---------------------------------------------------------------------

optsp :: Parser Opts
optsp = Opts
     <$> some (argument str (metavar "FILES..."))
     <*> (output1' <|> output2')
     <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Generate debug output" )
  where
    output1' =
      (\b -> if b then InplaceNoBackup else Stdout) <$>
        switch (long "inplace"
                <> short 'i'
               )
    output2' =
      InplaceBackup <$>
        strOption (long "inplace"
                <> metavar "SUFFIX"
                <> short 'i'
                <> help "Modify files in place (makes backup if SUFFIX supplied)"
                  )

-- ---------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= migrate
  where
    opts = info (helper <*> optsp)
      ( fullDesc
     <> progDesc "migrate FILE according to MRP"
     <> header "migrate - migrate source according to MRP" )
