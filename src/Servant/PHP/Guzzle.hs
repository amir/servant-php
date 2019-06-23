{-# LANGUAGE OverloadedStrings #-}

module Servant.PHP.Guzzle where

import Control.Lens hiding (List)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Servant.PHP.Internal
import Servant.Foreign
import Text.Casing (quietSnake)

guzzle :: PHPGenerator
guzzle a = "<?php\n"
  <> "use GuzzleHttp\\Client;\n"
  <> "class Servant {\n"
  <> "  private $client;\n"
  <> "  function __construct($url) {\n"
  <> "    $this->client = new Client(['base_uri' => $url]);\n"
  <> "  }\n"
  <> T.unlines (map generateGuzzlePHP a)
  <> "}"

generateGuzzlePHP :: Req Text -> Text
generateGuzzlePHP req = "\n"
  <> "  function "
  <> functionName
  <> "(" <> argsStr <> ") {\n"
  <> functionBody
  <> "  }"

  where
    functionName :: Text
    functionName = req ^. reqFuncName.snakeCaseL.to snake

    functionBody :: Text
    functionBody = "    $response = $this->client->request("
      <> "'" <> method <> "', "
      <> "\"" <> urlArgs <> "\", [\n"
      <> "      'query' => " <> queryArgs <> ",\n"
      <> "      'body' => " <> bodyOption <> ",\n"
      <> "      'headers' => [\n"
      <> T.intercalate ",\n" requestHeaders
      <> "\n      ]\n    ]);\n"
      <> "    return $response->getBody()->getContents();\n"

    method :: Text
    method =
      case req ^. reqMethod.to decodeUtf8' of
        Right m -> T.toUpper m
        Left _ -> "GET"

    segments :: [Segment Text]
    segments = filter isCapture paths
      where
        paths = toList (req ^. reqUrl.path)

    captures :: [Text]
    captures = view argPath . captureArg <$> segments

    argsStr :: Text
    argsStr = T.intercalate ", " $ var <$> args

    args :: [Text]
    args =
      captures
      ++ ((^. queryArgName.argPath) <$> queryparams)
      ++ bodyArg
      ++ rawHeaders

    queryArgs :: Text
    queryArgs =
      if null queryparams then
        "[]"
      else
        "[" <> phpParams "," queryparams <> "]"

    bodyArg :: [Text]
    bodyArg =
      case req ^. reqBody of
        Just _ -> ["body"]
        Nothing -> []

    bodyOption :: Text
    bodyOption =
      case req ^. reqBody of
        Just _ -> "$body"
        Nothing -> "null"

    contentType :: [Text]
    contentType =
      case req ^. reqBody of
        Just _ ->
          ["        'Content-Type' => 'application/json'"]
        Nothing -> []

    hs :: [HeaderArg Text]
    hs = req ^. reqHeaders

    requestHeaders :: [Text]
    requestHeaders = contentType ++ (requestHeader <$> rawHeaders)

    rawHeaders :: [Text]
    rawHeaders = (^. headerArg.argName._PathSegment) <$> hs

    queryparams :: [QueryArg Text]
    queryparams = req ^.. reqUrl.queryStr.traverse

    urlArgs :: Text
    urlArgs = req ^.. reqUrl.path.traverse & phpSegments

snake :: Text -> Text
snake = T.pack . quietSnake . T.unpack

var :: Text -> Text
var a = T.pack "$" <> snake a

phpSegments :: [Segment f] -> Text
phpSegments [] = ""
phpSegments [x] = "/" <> segmentToStr x
phpSegments (x:xs) = "/" <> segmentToStr x <> phpSegments xs

segmentToStr :: Segment f -> Text
segmentToStr (Segment st) = segmentTypeToStr st

segmentTypeToStr :: SegmentType f -> Text
segmentTypeToStr (Static s) = s ^. _PathSegment
segmentTypeToStr (Cap s) = "$" <> s ^. argName._PathSegment.to snake

phpParams :: Text -> [QueryArg f] -> Text
phpParams _ [] = ""
phpParams _ [x] = paramToStr x
phpParams s (x:xs) = paramToStr x <> s <> phpParams s xs

paramToStr :: QueryArg f -> Text
paramToStr qarg =
  case qarg ^. queryArgType of
    Normal -> "'" <> key <> "' => $" <> val
    Flag -> key
    List -> key <> "[]=" <> val
  where
    key = qarg ^. queryArgName.argName._PathSegment
    val = snake key

requestHeader :: Text -> Text
requestHeader header = "        '" <> header <> "' => " <> var header
