module FailCont.EmailValidator where
import FailCont.FailCont
import Data.Char (isAlphaNum, isSpace)
import Data.List (find)

data EmailError 
    = EmptyEmail
    | NoAtSymbol
    | NoLocalPart
    | NoDomainPart
    | InvalidLocalPart String
    | InvalidDomainPart String
    | MissingTLD
    | InvalidTLD String
    deriving (Show, Eq)

validateEmail :: String -> FailCont r EmailError String
validateEmail email = do
    nonEmpty <- toFailCont $ if null (filter (not . isSpace) email)
        then Left EmptyEmail
        else Right email
    parts <- toFailCont $ case break (== '@') nonEmpty of
        (_, "") -> Left NoAtSymbol
        (local, '@':domain) -> Right (local, domain)
        _ -> Left NoAtSymbol
    localPart <- validateLocalPart (fst parts)
    domainPart <- validateDomainPart (snd parts)    
    pure $ localPart ++ "@" ++ domainPart

validateLocalPart :: String -> FailCont r EmailError String
validateLocalPart local = toFailCont $ case () of
    _ | null local -> Left NoLocalPart
      | not (all isValidLocalChar local) -> Left $ InvalidLocalPart local
      | otherwise -> Right local
  where
    isValidLocalChar c = isAlphaNum c || c `elem` ".-_"

validateDomainPart :: String -> FailCont r EmailError String
validateDomainPart domain = do
    baseDomain <- toFailCont $ case () of
        _ | null domain -> Left NoDomainPart
          | not (all isValidDomainChar domain) -> Left $ InvalidDomainPart domain
          | otherwise -> Right domain
    let parts = break (== '.') baseDomain
    toFailCont $ case snd parts of
        "" -> Left MissingTLD
        ('.':tld) | not (isValidTLD tld) -> Left $ InvalidTLD tld
                  | otherwise -> Right baseDomain
        _ -> Left MissingTLD
  where
    isValidDomainChar c = isAlphaNum c || c `elem` ".-"
    isValidTLD tld = length tld >= 2 && all isAlphaNum tld

exampleEmails :: [(String, String)]
exampleEmails =
    [ ("test@example.com", "Valid email")
    , ("", "Empty email")
    , ("test.email", "Missing @ symbol")
    , ("@domain.com", "Missing local part")
    , ("test@", "Missing domain part")
    , ("test!@example.com", "Invalid characters in local part")
    , ("test@domain", "Missing TLD")
    , ("test@domain.c", "Invalid TLD")
    ]

runExamples :: IO ()
runExamples = mapM_ validateAndPrint exampleEmails
  where
    validateAndPrint (email, desc) = do
        putStrLn $ "\nTesting: " ++ email ++ " (" ++ desc ++ ")"
        print $ evalFailCont $ validateEmail email