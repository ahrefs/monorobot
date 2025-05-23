# Secrets

A secrets file stores sensitive information. Unlike the repository configuration file, it should not be checked into the monorepo's version control. Instead, store it locally at a location accessible by the bot.

# Options

**Example**

```json
{
    "repos": [
      {
        "url": "https://github.com/ahrefs/monorobot",
        "gh_token": "XXX"
      }
    ],
    "slack_access_token": "XXX"
}
```

| value | description | optional | default |
|-|-|-|-|
| `repos` | specify each target repository's url and its secrets | No | - |
| `slack_access_token` | slack bot access token to enable message posting to the workspace | Yes | try to use webhooks defined in `slack_hooks` instead |
| `slack_hooks` | list of channel names and their corresponding webhook endpoint | Yes | try to use token defined in `slack_access_token` instead |
| `slack_signing_secret` | specify to verify incoming slack requests | Yes | - |
| `buildkite_access_token` | Buildkite access token, used to query the Buildkite API for builds details | Yes | - |
| `buildkite_signing_secret` | specify to verify incoming Buildkite webhook requests | Yes | - |

Note that:
- either `slack_access_token` or `slack_hooks` must be defined. If both are present, the bot will send notifications using webhooks.
- the failed builds notifications require the `buildkite_access_token` to work

## `repos`

Specifies which repositories to accept events from, along with any repository-specific overrides to secrets.

```json
[
  {
    "url": "https://github.com/ahrefs/runner",
    "gh_token": "XXX"
  },
  {
    "url": "https://example.org/ahrefs/coyote",
    "gh_token": "XXX",
    "gh_hook_secret": "XXX"
  }
]
```

| value | description | optional | default |
|-|-|-|-|
| `url` | the repository url. | No | - |
| `gh_token` | specify to grant the bot access to private repositories; omit for public repositories | Yes | - |
| `gh_hook_secret` | shared secret token to authenticate the GitHub repository sending a notification | Yes | - |

### `repos`

Repository URLs should be fully qualified (include the protocol), with no trailing backslash.

### `gh_token`

Some operations, such as fetching a config file from a private repository, or the commit corresponding to a commit comment event, require a personal access token. Refer [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) for detailed instructions on token generation.

### `gh_hook_secret`

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks) for more information on securing webhooks with a secret token.

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

## `buildkite_access_token`
This token is used to get informations regarding builds details and components. It's required to use the failed builds notifications feature.

You also need to have one webhook configured on your pipelines with the `build.finished` scope. Configure the `buildkite_signing_secret` setting to validate the payloads if you require it.

To manage your API access tokens, visit your [personal settings page](https://buildkite.com/user/api-access-tokens) where you can create or edit them.

Admins can go to the [API Access Audit](https://buildkite.com/organizations/%7E/api-access-audit) page to review all tokens with access to the organization's data. You can also check each token's permissions and remove access if necessary.

## `buildkite_signing_secret`
This secret is used to [validate the payloads that Buildkite sends on the webhook](https://buildkite.com/docs/apis/webhooks#webhook-signature).

Webhooks can be added and configured on your organization's [Notification Services settings page](https://buildkite.com/organizations/-/services).
