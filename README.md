# xendit

A web client to communicate with Xendit API.
To use it, ensure that your monad satisfies the following constraint:
```
type WithXendit env err m = 
  ( MonadReader env m
  , HasXenditConfig env
  , MonadIO m
  , MonadError err m
  )
```
together with a function `ClientError -> err`
i.e. a function to convert client errors to your error type `err`. 

## Testing
The simple test suite expects a configuration file named `config.json`
in the root directory.
The configuration file should contain the following:
```
{
  "api_key": <your xendit API key>,
  "api_url": <xendit API url>,
  "callback_token": <your xendit callback token>
}
```
