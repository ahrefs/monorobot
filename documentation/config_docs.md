# Repository Configuration

A repository configuration file specifies how notifications should be handled for a given repository. It should be at the root of your monorepo in the main branch. The bot will look for a `notabot.json` file by default, but you can change this behavior with the `--config` flag.

When the bot receives its first incoming GitHub notification, it will query the repository URL to retrieve its configuration file. For subsequent notifications, it will use the cached configuration unless an update is detected.

To update the configuration, simply edit the configuration file and push your changes to GitHub. The bot will detect and apply those changes to the configuration, and will be reflected in the next request onwards.

Refer [here](https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/webhook-events-and-payloads) for more information on GitHub event payload structure.

# Configuration values

**example**
```json
{
    "main_branch_name": "develop",
    "status_rules": {
        ...
    },
    "prefix_rules": {
        ...
    },
    "label_rules": {
        ...
    }
}
```

| value | description | optional | default |
|-|-|-|-|
| `main_branch_name` | main branch used for the repo; filtering notifications about merges of main into other branches | Yes | - |
| `status_rules` | status rules config object | No | - |
| `label_rules` | label rules config object | No | - |
| `prefix_rules` | prefix rules config object | No | - |

## Status Config

**example**
```json
"status_rules": {
    "allowed_pipelines": [
        "default",
        "buildkite/pipeline2"
    ],
    "status": {
        "pending": false,
        "success": "once",
        "failure": true,
        "error": true,
        "cancelled": "^\\(Build #[0-9]+ canceled by .+\\|Failed (exit status 255)\\)$"
    }
},
```

| value | description | optional | default |
|-|-|-|-|
| `title` | if defines a whitelist of values for the github payload. If not specified, all is permitted. | Yes | - |
| `status` | a `status_state` config object | No | - |

### Status State

A json object with fields of bools for each status type.

| value | description | optional | default |
|-|-|-|-|
| `pending` | `true` to notify; `false` to ignore | No | - |
| `success` | `true` to notify; `false` to notify all; `"once"` to notify the first and ignore subsequent consecutive successes| No | - |
| `failure` | `true` to notify; `false` to ignore | No | - |
| `error` | `true` to notify; `false` to ignore | No | - |
| `cancelled` | provide regex to ignore `failure` notifications with a description that matches it | Yes | - |


## Label Options

**Label rules** apply to PR and issue notifications. If a payload matches multiple rules, they are all included.

**Example**
```json
"label_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": ["backend"],
            "channel": "backend"
        },
        {
            "allow": ["a1"],
            "channel": "a1-bot"
        },
        {
            "allow": ["a3"],
            "channel": "a3"
        },
        {
            "ignore": [
                "backend",
                "a1",
                "a3"
            ],
            "channel": "frontend-bot"
        }
    ]
},
```

| value | description | optional | default |
|-|-|-|-|
| `default_channel` | default channel to notify if no rules match | Yes | don't notify any channel |
| `rules` | list of `label_rule` objects | No | - |

### Label Rule

A **label rule** specifies whether or not a Slack channel should be notified, based on the labels present in the given payload. For each rule, `ignore` is a blacklist of labels that should not notify the rule's channel, and `allow` is a whitelist of labels that should. The `ignore` list takes precedence over the `allow` list. Both are optional; if neither are provided, the rule will always generate a notification for its channel.

| value | description | optional | default |
|-|-|-|-|
| `allow` | whitelist of labels that should match the rule | Yes | all labels allowed if no list provided |
| `ignore` | blacklist of labels that shouldn't match the rule | Yes | - |
| `channel` | channel to use as webhook if the rule is matched | No | - |

## Prefix Options

**Prefix rules** apply to push, commit comment, and status notifications. If a filename satisfies a prefix rule, the rule's channel will be notified. If a filename matches multiple rules, only the one that is matched by the *longest prefix* is included.

**Example**
```json
"prefix_rules": {
    "default_channel": "default",
    "rules": [
        {
            "allow": ["backend/a1"],
            "channel": "a1"
        },
        {
            "allow": [
                "backend/a5",
                "backend/a4"
            ],
            "channel": "backend"
        },
        {
            "channel": "all-push-events"
        }
    ]
},
```

### Prefix Rule

A **prefix rule** specifies whether or not a Slack channel should be notified, based on the filenames present in the commits associated with the given payload. The semantics for the `allow` and `ignore` fields are the same as those for label rules (see above).

| value | description | optional | default |
|-|-|-|-|
| `allow` | whitelist of file prefixes that should match the rule | Yes | all prefixes allowed if no list provided |
| `ignore` | blacklist of file prefixes that shouldn't match the rule | Yes | - |
| `channel` | channel to use as webhook if the rule is matched | No | - |
