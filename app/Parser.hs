module Parser (getOptions, Command (..), Options (..)) where

import Options.Applicative

data Command
  = Check String
  | AddWord String
  | RemoveWord String
  deriving (Show, Eq)

data Options = Options
  { getDictionaryFilename :: FilePath,
    getCommand :: Command
  }
  deriving (Show, Eq)

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "check"
        ( info
            (Check <$> strArgument (metavar "FILE"))
            (progDesc "Spellcheck a file")
        )
        <> command "dict" (info dictParser (progDesc "Manage dictionary"))
    )

dictParser :: Parser Command
dictParser =
  hsubparser
    ( command
        "add"
        ( info
            (AddWord <$> strArgument (metavar "STRING"))
            (progDesc "Add a word to the dictionary")
        )
        <> command
          "remove"
          ( info
              (RemoveWord <$> strArgument (metavar "STRING"))
              (progDesc "Remove a word from the dictionary")
          )
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> ( strOption
          ( long "dictionary"
              <> short 'd'
              <> metavar "DICTIONARY"
              <> help "Path to the dictionary file"
              <> value "dictionary.json"
              <> showDefault
          )
      )
    <*> commandParser

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    idm

getOptions :: IO Options
getOptions = execParser opts
