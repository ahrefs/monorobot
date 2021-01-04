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
- `check_slack <SLACK_PAYLOAD>`: read a Slack notification from a file and send it to a channel (used for testing)

## Getting Started

1. Commit a **repository configuration** file to the root of your target repository.
2. Place a **secrets** file locally on the server.
3. Configure GitHub
    1. If targeting a private repository, set up a [personal access token](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) with `repo` scope and store it in the `gh_token` field of the secrets file.
    2. [Create a webhook](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/creating-webhooks#setting-up-a-webhook) for the repository you are targeting. Set the *Payload URL* to be `<server_domain>/github`.
    3. You can optionally [secure the webhook](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) with a token, and store it in the `gh_hook_token` field of the secrets file.
4. Configure Slack
    1. [Create a Slack app](https://api.slack.com/apps?new_app=1).
    2. Click "Install to Workspace", and when prompted to grant permissions to your workspace, click "Allow".
    3. Set up notifications with one of the following methods:
        - **Web API (recommended):** To use Slack's [Web API](https://api.slack.com/web), click on "OAuth & Permissions" in your app dashboard's sidebar. Give your bot a *Bot Token Scope* of `chat:write`. Copy the generated OAuth access token (`xoxb-XXXX`) to the `slack_access_token` field of your secrets file. This token is used by the bot to authenticate to the workspace, and remains valid until the token is revoked or the app is uninstalled.
        - **Incoming Webhooks:** To use [incoming webhooks](https://api.slack.com/messaging/webhooks), enable them in your app dashboard and create one for each channel you want to notify. Store them in the `slack_hooks` field of your secrets file. If you decide to notify additional channels later, you will need to update the secrets file with the new webhooks and restart the server.


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
