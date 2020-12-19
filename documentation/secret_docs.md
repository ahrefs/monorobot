# About

Secret file is where sensitive information such as the urls used for webhooks and other tokens are stored.

# Secret Values

**example**
```json
{
    "slack_hooks": [
        {
            "url": "https://slack_webhook_url",
            "channel": "default"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "a1"
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
            "channel": "a1-bot"
        },
        {
            "url": "https://slack_webhook_url",
            "channel": "a3"
        }
    ]
}
```

| value | description | optional | default |
|-|-|-|-|
| `slack_hooks` | list of webhook config objects | No | - |
| `gh_token` | specify to grant the bot access to private repositories; omit for public repositories | Yes | - |
| `gh_hook_token` | specify to ensure the bot only receives GitHub notifications from pre-approved repositories | Yes | - |

## `gh_token`

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


## `gh_hook_token`
For more information on `gh_hook_token` see [developer.github.com/webhooks/securing](https://developer.github.com/webhooks/securing/)

## Webhook Config

Channels that are defined in rules in config will be mapped to urls defined in the webhook

| value | description | optional | default |
|-|-|-|-|
| `url` | url to call to send the message | No | - |
| `channel` | name of the channel where the message will be posted as used in config | No | - |
