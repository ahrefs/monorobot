# Secrets

A secrets file stores sensitive information. Unlike the repository configuration file, it should not be checked into the monorepo's version control. Instead, store it locally at a location accessible by the bot.

# Options

**Example**

```json
{
    "gh_token": "",
    "slack_access_token": ""
}
```

| value | description | optional | default |
|-|-|-|-|
| `gh_token` | specify to grant the bot access to private repositories; omit for public repositories | Yes | - |
| `gh_hook_token` | specify to ensure the bot only receives GitHub notifications from pre-approved repositories | Yes | - |
| `slack_access_token` | slack bot access token to enable message posting to the workspace | Yes | try to use webhooks defined in `slack_hooks` instead |
| `slack_hooks` | list of channel names and their corresponding webhook endpoint | Yes | try to use token defined in `slack_access_token` instead |
| `slack_signing_secret` | specify to verify incoming slack requests | Yes | - |

Note that either `slack_access_token` or `slack_hooks` must be defined. If both are present, the bot will send notifications using webhooks.

## `gh_token`

Some operations, such as fetching a config file from a private repository, or the commit corresponding to a commit comment event, require a personal access token. Refer [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) for detailed instructions on token generation.

## `gh_hook_token`

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) for more information on securing webhooks with a token.

## `slack_access_token`

Required for:
- Notification sending via Web API
- Link unfurling

You can obtain a bot token from the "OAuth & Permissions" in your app dashboard's sidebar. Note that you need a *bot* token (`xoxb-XXXX`), not a *user* token (`xoxp-XXXX`).
See [here](https://api.slack.com/authentication/basics#start) for creating/installing an app and requesting scopes.

Give it the following scopes:
- For notifications - [`chat:write`](https://api.slack.com/scopes/chat:write) (per-channel authorization) or [`chat:write.public`](https://api.slack.com/scopes/chat:write.public) (authorization to all channels)
    - Note: If you use the `chat:write` scope, add the bot to each channel you want to notify.
- For link unfurling - [`links:read`](https://api.slack.com/scopes/links:read) and [`links:write`](https://api.slack.com/scopes/links:write) (also see **Link Unfurling** in main README)

## `slack_hooks`

Required for:
- Notification sending via webhooks

Expected format:

```json
[
    {
        "channel": "channel name",
        "url": "webhook url"
    },
    {
        "channel": "channel name",
        "url": "webhook url"
    },
    ...
]
```

Refer [here](https://api.slack.com/messaging/webhooks) for obtaining a webhook for a channel.
