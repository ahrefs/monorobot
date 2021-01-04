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

Note that either `slack_access_token` or `slack_hooks` must be defined.

## `gh_token`

Some operations, such as fetching a config file from a private repository, or the commit corresponding to a commit comment event, require a personal access token. Refer [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) for detailed instructions on token generation.

## `gh_hook_token`

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) for more information on securing webhooks with a token.

## `slack_access_token`

Refer [here](https://api.slack.com/authentication/oauth-v2) for obtaining an access token via OAuth.

## `slack_hooks`

*Note: If `slack_access_token` is also defined, the bot will authenticate over Slack's Web API and this option will not be used.*

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
