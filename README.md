# Monorobot

A Slackbot for GitHub monorepos. Configure how repo notifications should be routed to specified Slack channels based on file prefixes, issue/PR labels, and CI build statuses.

## Setting Up

Install dependencies via OPAM.

```sh
opam install --deps-only .
```

Then, build with Dune.

```sh
make
```

## Running

Run the `_build/default/src/monorobot.exe` binary. The following commands are supported.

- `run`: Launch the HTTP server
- `check_gh <GH_PAYLOAD>`: read a Github notification from a file and display the actions that will be taken (used for testing)
- `check_slack <SLACK_PAYLOAD> <SLACK_WEBHOOK>`: read a Slack notification from a file and send it to a webhook (used for testing)

### Documentation

The bot expects two configuration files to be present.

* [Repository configuration](./documentation/config_docs.md)
* [Secrets](./documentation/secret_docs.md)

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
