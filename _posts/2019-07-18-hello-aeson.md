---
layout: post
title: Aeson 소개
categories:
- Haskell
feature_image: "https://picsum.photos/id/872/1300/200"
---

JSON(JavaScript Object Notation)은 속성-값 쌍 혹은 키-값 쌍으로 이루어진 데이터 오브젝트를 전송하기 위해 사람이 읽을 수 있는 텍스트를 사용하는 개방형 표준 포맷이다. 자바스크립트를 사용한 홈페이지를 만들어 봤거나 파이썬이나 자바를 사용해 크롤링을 시도해 봤다면 한번쯤 다뤄봤을 것이다. JSON은 웹사이트 상에서 널리 사용되기 때문에 쉽게 다룰수 있으면 인터넷에서 데이터를 받아오거나 전송하기 쉬워진다.

# Aeson 사용하기
`Aeson`은 JSON을 파싱하기 위해 하스켈(Haskell)에서 사용하는 라이브러리이다. Aeson을 하스켈에서 사용하기 위해 `Data.Aeson` 를 `Import` 해줘야 한다. 또한 모듈 상단에 `{-# LANGUAGE OverloadedStrings #-}` 를 써줘서  `OverloadedStrings` 을 익스텐션 해야만 ByteString을 사용할 수 있다.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
```

`encode`를 사용해 Object로 이루어진 JSON을 String으로 변환할 수 있다.  
`decode`를 사용해 String으로 이루어진 JSON을 Object로 변환할 수 있다.

```haskell
> :type encode
encode :: forall a. ToJSON a => a -> ByteString

>:type decode
decode :: forall a. FromJSON a => ByteString -> Maybe a
```

Json Object를 만들기 위해선

```haskell
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int]
  "string" .= "hello" ]
```

```haskell
> :type val
val :: Value

> val
{
    "string": "hello",
    "boolean": true,
    "numbers": [
        1,
        2,
        3
    ]
}
```

IHaskell에선 JSON Object인 `Value`에 대한 디스플레이 인스턴스를 위와 같이 보여주지만, GHCi 에선 아래와 같이 출력된다.

```haskell
> putStr . show $ val
Object (fromList [("string",String "hello"),("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])])
```
# 인코딩 (Object -> ByteString)

```haskell
> :type encode val
encode val :: ByteString

> encode val
"{\"string\":\"hello\",\"boolean\":true,\"numbers\":[1,2,3]}"
```
# 디코딩 (ByteString -> Object)

```haskell
> decode "{\"string\":\"hello\",\"boolean\":true,\"numbers\":[1,2,3]}"  -- 타입
Nothing
```
```haskell
> decode "{\"string\":\"hello\",\"boolean\":true,\"numbers\":[1,2,3]}" :: Maybe Value
Just (Object (fromList [("string",String "hello"),("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])]))
```


```haskell
Just val == decode "{\"string\":\"hello\",\"boolean\":true,\"numbers\":[1,2,3]}"
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
