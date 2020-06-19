# aws-lambda-haskell-runtime-wai ![Haskell CI](https://github.com/eir-forsakring/aws-lambda-haskell-runtime-wai/workflows/Haskell%20CI/badge.svg)

## Quick start

1. Set up your project to use AWS Lambda by following the instructions on the [aws-lambda-haskell-runtime website](https://theam.github.io/aws-lambda-haskell-runtime/).
2. Use the `waiHandler` function from `AWS.Lambda.Wai` to convert your [`wai`](https://hackage.haskell.org/package/wai) application to a handler. There are two ways to do this.

```haskell
-- 1. Pass in the initializeApplicationFunction
-- this will call initializeApplication per each call
handler :: WaiHandler ()
handler = waiHandler initializeApplication

-- Wai application initialization logic
initializeApplication :: IO Application
initializeApplication = ...
``` 

```haskell
-- 2. Store the Application inside your custom context and provide a getter function
-- this will initialize the application once per cold start and keep it alive while the lambda is warm
handler :: WaiHandler MyAppConfig
handler = waiHandler' getWaiApp

data MyAppConfig =
  MyAppConfig
    { getWaiApp :: Application }
```