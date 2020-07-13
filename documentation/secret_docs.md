# About

Secret file is where sensitive information such as the urls used for webhooks and other tokens are stored.

# Secret Values

| value | description | optional | default |
|-|-|-|-|
| `slack_channels` | list of webhook config objects | No | - |
| `gh_token` | must not be specified for public repositories | Yes | - |
| `gh_webhook_secret` | if not specified signatures will not be checked | Yes | - |

## Webhook Config

Channels that are defined in rules in config will be mapped to urls defined in the webhook

| value | description | optional | default |
|-|-|-|-|
| `url` | url to call to send the message | No | - |
| `channel` | name of the channel where the message will be posted as used in config | No | - |
