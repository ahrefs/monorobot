# Monorobot

A Slackbot for GitHub monorepos. Configure how repo notifications should be routed to specified Slack channels based on file prefixes, issue/PR labels, and CI build statuses.

Check out the [accompanying blog post](https://tech.ahrefs.com/monorobot-a-slack-bot-for-monorepos-374260e2ca43) for an overview of Monorobot.

## Setting Up

Install via OPAM:

```
opam install monorobot
```

Or install dependencies and build locally:

```sh
opam install --deps-only .
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
       To use the "Project Owners" feature, the token owner must have triage role or above for the repository.
    2. [Create a webhook](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/creating-webhooks#setting-up-a-webhook) for the repository you are targeting. Set the *Payload URL* to be `<server_domain>/github`.
    3. You can optionally [secure the webhook](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) with a token (e.g. use `head -c 30 /dev/urandom | base64`), and store it in the `gh_hook_secret` field of the secrets file.
4. Configure Slack
    1. [Create a Slack app](https://api.slack.com/apps?new_app=1).
    2. Click "Install to Workspace", and when prompted to grant permissions to your workspace, click "Allow".
    3. Set up notifications with one of the following methods:
        - **Web API (recommended):** To use Slack's [Web API](https://api.slack.com/web), click on "OAuth & Permissions" in your app dashboard's sidebar. Give your bot a *Bot Token Scope* of [`chat:write`](https://api.slack.com/scopes/chat:write) (for per-channel authorization) or [`chat:write.public`](https://api.slack.com/scopes/chat:write.public) (for authorization to all channels). Also give `chat:write.customize` (to show branch name as bot username). If you want to send direct messages, also add the `users:read.email` scope, whichis necessary to find the user id matching the email of a commit author. Copy the generated bot token (`xoxb-XXXX`) to the `slack_access_token` field of your secrets file. This token is used by the bot to authenticate to the workspace, and remains valid until the token is revoked or the app is uninstalled. If you use the `chat:write` scope, add the bot to each channel you want to notify.
        - **Incoming Webhooks:** To use [incoming webhooks](https://api.slack.com/messaging/webhooks), enable them in your app dashboard and create one for each channel you want to notify. Store them in the `slack_hooks` field of your secrets file. If you decide to notify additional channels later, you will need to update the secrets file with the new webhooks and restart the server.


### Link Unfurling

You can configure Monorobot to [unfurl GitHub links](https://api.slack.com/reference/messaging/link-unfurling) in Slack messages. Currently, commit, pull request, and issue links are supported.

Note: The `slack_access_token` must be configured in your secrets file for link unfurling. See previous section for details.

1. Give your app `links:read` and `links:write` [permissions](https://api.slack.com/apps).
1. Configure your app to [support the Events API](https://api.slack.com/events-api#prepare). During the [url verification handshake](https://api.slack.com/events-api#the-events-api__subscribing-to-event-types__events-api-request-urls__request-url-configuration--verification__url-verification-handshake), you should tell Slack to direct event notifications to `<server_domain>/slack/events`. Ensure the server is running before triggering the handshake.
1. [Register the GitHub domains](https://api.slack.com/reference/messaging/link-unfurling#configuring_domains) you want to support.

### Slack mentions

Monorobot will also try to match mentioned GitHub handles (e.g., in PR/issue/commit comments) to Slack emails. Where there is a match between canonicalised GitHub handle and canonicalised Slack email, the GitHub mention in Monorobot's notification will be replaced with a Slack mention. This feature requires `slack_access_token` to be configured, and your app must also have the `users:read` permission. See the previous section for how to set this up.

If your canonicalised GitHub handle is different from your canonicalised Slack email, or if you want to override this default matching scheme, you can create a manual mapping in your repository configuration under the `user_mappings` option. See the [documentation](./documentation/config_docs.md) for details.

### Slack notifications to a channel dedicated to failed builds

When a pipelinel is configured with a failed builds channel, Monorobot will send notifications to that Slack channel when a build fails for the main branch in the configured pipeline. This notification will mention the commit author and the build steps broken by the commit. This is useful for tracking the status of builds in Slack, and for getting notifications when a build fails.

Currently, this feature is only supported for Buildkite builds.

The logic is as follows:
1. We receive a notification from GitHub for each step in the CI build lifecycle, including both individual steps and the entire build. We maintain a log of failed builds and failed steps for each build.
2. When a notification for a failed build is received, we evaluate the failures by reviewing that build's history and compare it with the previous builds.
3. We identify which steps were newly broken by the current build, i.e, did not fail in previous builds. We assume that previous builds still running will succeed while we haven't received a notifications for failed steps, so we treat any newly broken steps in the current build as broken by the current build.
4. If there are newly broken steps, we notify the configured failed builds channel, including the commit author's name and the broken steps.
5. If no steps were newly broken in the current build, we do not send a notification.
6. In summary, only additional breakages are notified and notifications are sent as the builds finish. This means that they are not necessarily sent in the same order as the builds were started.

Some examples:
- Build #1 breaks steps `a` and `b`, build #2 then breaks steps `a` and `c`.
  - This will notifify the channel for build #1 regarding the steps `a` and `b`, and for build #2 regarding the step `c`.
- Build #1 breaks steps `a` and `b`, build #2 breaks step `a` and `c`, build #3 breaks step `a` and `b`.
  - Similar as above, but we assume build #2 fixed step `b` and build #3 broke it again. So we notify the channel like above for the first two builds, and for build #3 regarding the step `b` only.
- Build #1 is running for a long time, breaks step `a` in the process and we get the notification (with which we update the builds state), build #2 breaks step `a` and `b`. Build #2 finishes before build #1.
  - Will notify the channel for build #2 regarding step `b`. Then build #1 finishes and we notify the channel for the steps that build #1 newly broke.
  - Edge case: if by the time build #2 finishes we didn't receive any notification for a failed step in build #1, we will notify the channel for build #2 for the steps `a` and `b`, and then again for build #1.

### Documentation

Commit a configuration file to the root of each repository you want to support, and add a secrets file on the bot server itself. Read on for instructions to set up each file:

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
