# Secrets

A secrets file stores sensitive information. Unlike the repository configuration file, it should not be checked into the monorepo's version control. Instead, store it locally at a location accessible by the bot.

# Options

**Example**

```json
{
    "slack_hooks": [
        {
            "url": "https://slack_webhook_url",
            "channel": "default"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "aa"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "backend"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "all-push-events"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "frontend-bot"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "aa-git"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "siren"
        }
    ]
}
```

| value | description | optional | default |
|-|-|-|-|
| `slack_hooks` | list of channel names (`channel`) and their corresponding webhook endpoint (`url`) | No | - |
| `gh_token` | specify to grant the bot access to private repositories; omit for public repositories | Yes | - |
| `gh_hook_token` | specify to ensure the bot only receives GitHub notifications from pre-approved repositories | Yes | - |

## `gh_token`

Some operations, such as fetching a config file from a private repository, or the commit corresponding to a commit comment event, require a personal access token. Refer [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) for detailed instructions on token generation.

## `gh_hook_token`

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) for more information on securing webhooks with a token.
