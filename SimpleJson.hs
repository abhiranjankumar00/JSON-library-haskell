--file: SimpleJson.hs

module SimpleJson (
  JValue (..),
  getString,
  getDouble, 
  getInt, 
  getBool, 
  getObject, 
  getArray,
  isNull
) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull 
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString x) = Just x
getString _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber x) = Just x
getDouble _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber x) = Just $ truncate x
getInt _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool x) = Just x
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject x) = Just x
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray x) = Just x
getArray _ = Nothing

isNull :: JValue -> Bool
isNull x = x == JNull
