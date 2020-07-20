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

### Config File

The config file is where the variables affecting the behaviour of notabot are defined. It mainly consists of three categories of rules to filter notifications; status, prefix and label, in each there may be more than one type of rule.

For details about each field in the config, please refer to [notabot.atd](./lib/notabot.atd).

Here is an example of a [config file](./test/notabot.json).


### Secret file

Secret file is where sensitive information such as the urls used for webhooks and other tokens are stored.

#### Token generation

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

#### Webhook Secret

For more information on `gh_webhook_secret` see [developer.github.com/webhooks/securing](https://developer.github.com/webhooks/securing/)

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
