# Notabot

Notifications bot server to receive notifications from webhooks and post them to slack

## Setting Up

Install dependencies, if needed:

```sh
opam install sha atdgen httpaf httpaf-lwt-unix base stdio lwt
```

Building notabot should be as easy as doing

```sh
make build
```

## Running Notabot

To run, make sure that you know the `User-Agent` and `X-Hub-Signature` that github will be using on the set up webhooks.

You can check those on the repo: https://github.com/ahrefs/{repo}/settings/hooks/{hookid}. (Repo > settings > webhooks > hook in question).

At the end of that screen you have a `Recent deliveries` section where you can see the webhooks requests details.

To test notabot run with

```sh
SHA1_SIG={a signature value you define} GITHUB_AGENT={whatever user agent you define} make start
```

Then on another tab run:
(you will need to update the values of `SHA1_SIG` and `HITHUB_AGENT` both on the payload and the start command)

```sh
curl -X POST \
  http://localhost:8080/github \
  -H 'Content-Type: application/json' \
  -H 'User-Agent: {whatever user agent you define}' \
  -H 'X-Github-Event: pull_request' \
  -H 'X-Hub-Signature: {a signature value you define}' \
  -H 'cache-control: no-cache' \
  -d @mock_payloads/pr_notification.json
```

There are more payloads which will use a different `X-Github-Event` signature:

```
pull_request => mock_payloads/pr_notification.json
push => mock_payloads/push_notification.json
check_suite => mock_payloads/ci_notification.json
```
