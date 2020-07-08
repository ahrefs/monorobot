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

and use resulting `_build/default/src/notabot.exe` binary.

## Running

Configuration is read at startup from `notabot.json` and `secrets.json` files
according to `lib/notabot.atd` schema.

### Token generation

Some event notifications (e.g., status, commit comment) require a personal token to be addded to the configuration. To create a personal token, take the following steps:
1. Verify your email address, if needed.
1. In the upper-right corner of any page, click your profile photo, then click **Settings**.
1. In the left sidebar, click **Developer settings**.
1. In the left sidebar, click **Personal access tokens**.
1. Click **Generate new token**.
1. Give your token a descriptive name in the **Note** section.
1. Grant ***repo*** scope.
1. Click **Generate token**.
1. Copy the token to `secrets.json` file in a `gh_token` field.

For more detailed instructions on token generation, refer to https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line.

### Documentation

* [config](./documentation/config.md)
* [secret](./documentation/secret.md)

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
