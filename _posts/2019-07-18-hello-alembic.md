---
layout: post
title: Hello Alembic
categories:
- General
feature_image: "https://picsum.photos/id/872/1300/200"
---

Some examples from https://artyom.me/aeson and others in a IHaskell notebook


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Types
import Data.Text

:type encode
:type decode
```
```
encode :: forall a. ToJSON a => a -> ByteString
decode :: forall a. FromJSON a => ByteString -> Maybe a</span>
```


```haskell
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int] ]
```

```haskell
:type val
val
```
```
    {
        "boolean": true,
        "numbers": [
            1,
            2,
            3
        ]
    }
```


IHaskell has its own Display instnace for the `Value`s of JSON objects, as displayed above.
A plain GHCi would show such `Value`s as bellow.


```haskell
putStr . show $ val
```
```
Object (fromList [("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])])
```

```haskell
:type encode val
encode val
```
```encode val :: ByteString
"{\"boolean\":true,\"numbers\":[1,2,3]}"
```

```haskell
decode "{\"boolean\":true,\"numbers\":[1,2,3]}" -- not enough type information
```
```
Nothing
```


```haskell
decode "{\"boolean\":true,\"numbers\":[1,2,3]}" :: Maybe Value
```
```
Just (Object (fromList [("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])]))
```


```haskell
Just val == decode "{\"boolean\":true,\"numbers\":[1,2,3]}"
```
```
True
```


```haskell
Just val == decode (encode val)
```
```
True
```

Selecting specific fields from JSON


```haskell
v = object [ -- list of personal info like an addressbook
        "John" .= object [ "age" .= 20, "address" .= "Calgary" ],
        "Jane" .= object [ "age" .= 19, "address" .= "Tornoto" ],
        "Kate" .= object [ "age" .= 15, "address" .= "Montreal" ],
        "Jack" .= object [ "age" .= 22, "address" .= "Vancouver" ] ] 
```


```haskell
:type withObject
```
```
withObject :: forall a. String -> (Object -> Parser a) -> Value -> Parser a
```


```haskell
ageOf name = withObject "addressbook" $ \o -> do
  info <- o .: name
  age <- info .: "age"
  return age
```


```haskell
:type ageOf
:type ageOf "Kate"
:type ageOf "Kate" v
```
```
ageOf :: forall a. FromJSON a => Text -> Value -> Parser a
ageOf "Kate" :: forall a. FromJSON a => Value -> Parser a
ageOf "Kate" v :: forall a. FromJSON a => Parser a
```


```haskell
:type parseMaybe
:type parseEither
```
```
parseMaybe :: forall a b. (a -> Parser b) -> a -> Maybe b
parseEither :: forall a b. (a -> Parser b) -> a -> Either String b
```



```haskell
parseMaybe (ageOf "Paul") v
parseMaybe (ageOf "Kate") v -- not enough type information
parseMaybe (ageOf "Kate") v :: Maybe Bool  -- wrong type
parseMaybe (ageOf "Kate") v :: Maybe Int
```
```
Nothing
Nothing
Nothing
Just 15
```


```haskell
parseEither (ageOf "Paul") v
parseEither (ageOf "Kate") v -- not enough type information defaults to ()
parseEither (ageOf "Kate") v :: Either String Bool  -- wrong type
parseEither (ageOf "Kate") v :: Either String Int
```
```
Left "Error in $: key \"Paul\" not present"
Left "Error in $.age: expected (), encountered Number"
Left "Error in $.age: expected Bool, encountered Number"
Right 15
```

Often better to give a more speicfic type signature


```haskell
ageOf' :: Text -> Value -> Parser Int
ageOf' name = withObject "addressbook" $ \o -> do
  info <- o .: name
  age <- info .: "age"
  return age
```


```haskell
parseEither (ageOf' "Kate") v
parseEither (ageOf' "Jane") v
```
```
Right 15
Right 19
```
