# servant-php

## Servant API
```haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
```

## Generated PHP Client
```php
<?php
use GuzzleHttp\Client;
class Servant {
  private $client;
  function __construct($url) {
    $this->client = new Client(['base_uri' => $url]);
  }

  function get_position_by_x_by_y($x, $y) {
    $response = $this->client->request('GET', "/position/$x/$y", [
      'query' => [],
      'body' => null,
      'headers' => [

      ]
    ]);
    return $response->getBody()->getContents();
  }

  function get_hello($name) {
    $response = $this->client->request('GET', "/hello", [
      'query' => ['name' => $name],
      'body' => null,
      'headers' => [

      ]
    ]);
    return $response->getBody()->getContents();
  }

  function post_marketing($body) {
    $response = $this->client->request('POST', "/marketing", [
      'query' => [],
      'body' => $body,
      'headers' => [
        'Content-Type' => 'application/json'
      ]
    ]);
    return $response->getBody()->getContents();
  }
}

```
