import Data.Char

type Message                                                        = [Token]
type Token                                                          = [Char]

specialcases                                                        :: [Char]
specialcases                                                        =  ['a','b','m','o','v','x']

emote                                                               :: Token -> Bool
emote (x:xs) | length xs > 0  && all (isValid) (x:xs)               = True
             | length xs == 0 && x `elem` specialcases              = True
             | otherwise                                            = False
             
isValid                                                             :: Char -> Bool
isValid x                                                           = isAlphaNum x || x == '_'
             
detokenise                                                          :: Message -> String
detokenise                                                          = concat

tokenise                                                            :: String -> Message
tokenise []                                                         = []
tokenise (':':xs) | ':' `elem` xs && emote (takeWhile(/= ':') xs)   = (':' : (takeWhile(/= ':') xs) ++ [':']) : tokenise (tail(dropWhile(/= ':') xs))
                  | otherwise                                       = [':'] : tokenise xs
tokenise (x:xs)                                                     = [x] : tokenise xs

spoiler                                                             :: Message -> Message
spoiler []                                                          = []
spoiler (x:xs)                                                      = ["||" ++ x ++ "||"] ++ spoiler xs

spoil                                                               :: String -> String
spoil                                                               = detokenise . spoiler . tokenise