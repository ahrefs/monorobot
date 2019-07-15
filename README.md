# Notabot

Notifications bot, currently only handling github webhook events, specifically commit pushed, pull request reviewed requested and CI build finished.

## Instalation

To run Notabot you will need to install the following dependencies:

```sh
opam install sha atdgen httpaf httpaf-lwt-unix base stdio lwt
```

## Setting up

You will need a webhook set up, pointing to your deployment env and port, at the `/github` route.

When using the github webhooks, you will need to take notice of the request headers sent, namely `X-Hub-Signature` and `User-Agent`, which will need to be passed in as environment variables for the server to do its validation.

You can test Notabot locally by using `ngrok` to generate an internet facing endpoint pointing to your local machine. More on that [here](https://developer.github.com/webhooks/configuring/).

## Running

To run the server do:

```sh
SHA1_SIG={your X-Hub-Signature} GITHUB_AGENT={your github webhook User-Agent header} dune exec ./server.exe
```
