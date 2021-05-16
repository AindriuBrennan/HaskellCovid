module Params (Params (..), cmdLineParser) where

import Options.Applicative
import Data.Text (Text, strip)

data Params = Params {
                fname :: FilePath,
                chart :: Bool,
                htmlFile :: Maybe FilePath,
                silent :: Bool
                }


mkParams :: Parser Params
mkParams = 
    Params <$>
                strArgument
                  (metavar "FILE" <> help "CSV file name")
            <*> switch

                  (long "chart" <> short 'c' <>
                    help "generate chart")
            <*> optional (strOption $ 
                  long "html" <> metavar "FILE" <>
                  help "generate html report")
            <*> switch
                 (long "silent" <> short 's' <>
                  help "dont print information")


cmdLineParser :: IO Params
cmdLineParser = execParser opts
    where 
        opts = info (mkParams <**> helper)
                    (fullDesc <> progDesc "Processing Covid data")