--file: PutJson.hs

module PutJson where

import Data.List (intercalate)
import Data.Char (toLower)
import SimpleJson

renderJValue :: JValue -> String
renderJValue (JString x) = show x
renderJValue (JNumber x) = show x
renderJValue (JBool x) = map toLower. show $ x
renderJValue (JNull) = "null"

--renderJValue (JObject x) = (\y -> "{" ++ y ++ "}"). intercalate ", ". renderJObject $ x
--  where
--    renderJObject :: [(String, JValue)] -> [String]
--    renderJObject [] = []
--    renderJObject ((key, value): xs) = (key ++ ": " ++ renderJValue value): renderJObject xs

renderJValue (JObject x) = (\y -> "{" ++ y ++ "}"). intercalate ", ". map (\(key, value) -> (key ++ ":" ++ renderJValue value)) $ x

renderJValue (JArray a) = (\x -> "[" ++ x ++ "]"). intercalate ", ". map renderJValue $ a

putJValue :: JValue -> IO()
putJValue v = putStrLn. renderJValue $ v

stringify :: JValue -> String
stringify = show

parse :: String -> JValue
parse x = read x :: JValue
