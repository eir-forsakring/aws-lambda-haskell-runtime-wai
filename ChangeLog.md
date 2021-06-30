# Changelog for aws-lambda-haskell-runtime-wai

## 2.0.2

* Make registering multiple wai handlers possible (via `waiHandler`).
* Make registering multiple handlers with multiple Wai Applications possible (via `runMultipleWaiApplications`).
* Use version `4.1.1` of `aws-lambda-haskell-runtime`.

## 2.0.1

* Use version `4.1.0` of `aws-lambda-haskell-runtime` that fixes [#101](https://github.com/theam/aws-lambda-haskell-runtime/issues/101).

## 2.0.0

* Use version [`4.0.0`](https://github.com/theam/aws-lambda-haskell-runtime/pull/97) of `aws-lambda-haskell-runtime`.
* New handler types that allow you to support ALB or even API Gateway + ALB at once.

## 1.0.3

* Now taking the Wai request path from API Gateway's 'proxy' path parameter. This prevents the resource path from messing up your Wai application routing.

## 1.0.2

* Switched to aws-lambda-haskell-runtime 3.0.3

## 1.0.1

* Removed forgotten print statements
* Applied a [temporary hotfix](https://github.com/eir-forsakring/aws-lambda-haskell-runtime-wai/pull/4) for handling query parameters
