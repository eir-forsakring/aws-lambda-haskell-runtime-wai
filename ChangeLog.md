# Changelog for aws-lambda-haskell-runtime-wai

## 1.0.3

* Now taking the Wai request path from API Gateway's 'proxy' path parameter. This prevents the resource path from messing up your Wai application routing.

## 1.0.2

* Switched to aws-lambda-haskell-runtime 3.0.3

## 1.0.1

* Removed forgotten print statements
* Applied a [temporary hotfix](https://github.com/eir-forsakring/aws-lambda-haskell-runtime-wai/pull/4) for handling query parameters
