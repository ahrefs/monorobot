# Notabot

Notifications bot server to receive notifications from webhooks and post them to slack

## Setting Up

Install dependencies, if needed:

```sh
opam install --deps-only .
```

Build with

```sh
make
```

## Running

Currently configuration is defined by environment variables `GH_WEBHOOK_SECRET_TOKEN` and `SLACK_WEBHOOK_URL`.
Then run with `make start` or install/launch `_build/default/src/notabot.exe` binary as needed.

## Testing (development)

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
